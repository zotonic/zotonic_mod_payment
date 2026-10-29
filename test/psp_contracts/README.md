# PSP HTTP contract tests

Run from a compiled Zotonic umbrella checkout with the five PSP modules installed:

```sh
apps_user/zotonic_mod_payment/test/run-psp-contracts.sh
```

Pass a provider name (`mollie`, `stripe`, `paypal`, `buckaroo`, or `sovergn`) to
run only that provider's contract tests. Only that PSP and `mod_payment` need to
be checked out. With no argument, the runner tests all five providers.

The runner compiles test-only exports into a temporary directory and removes it
on exit. It needs Erlang, the normal Zotonic dependencies and `meck` (already a
Zotonic core development dependency). No site, database, credentials or network
access is needed. Expected failure tests emit adapter error logs.

## Contract sources

Fixtures were authored from the PSP documentation, consulted on 2026-10-29,
with synthetic identifiers, amounts and merchant details. Each fixture records
its source URL. They are not recordings or snapshots of our adapter's output.

| PSP | Documented operations mocked |
| --- | --- |
| Mollie | Create/get/list payments; create/get customers; list mandates; create/list/cancel customer subscriptions |
| Stripe | Create/retrieve/expire Checkout Sessions |
| PayPal | OAuth client credentials; create/get/capture orders; get captures; verify webhook signatures |
| Buckaroo | Create transaction; get transaction status |
| Sovergn | OAuth client credentials; create checkout session; get merchant status by checkout session |

Additional authentication and error references:

- [Mollie error responses](https://docs.mollie.com/reference/handling-errors)
- [Stripe API errors](https://docs.stripe.com/api/errors)
- [PayPal OAuth](https://developer.paypal.com/api/rest/authentication/)
- [Buckaroo JSON authenticator](https://docs.buckaroo.io/docs/json-authenticator)
- [Sovergn merchant OpenAPI](https://beta.sovergnllc.com/api-docs/openapi.json)

The Sovergn checkout fixture uses the published `oauthNonAopUsd` example. Its
status fixture uses `MerchantPaymentStatusView`, including the required privacy
boundary flags. OAuth uses the documented Basic authentication and scope subset.
Sovergn's mock implements no listing or discovery operation.

Mollie collections use `_embedded`, absolute next-page links and a final null
`next`. Cancellation returns a subscription object with HTTP 200. Mollie's
initial URLs currently use the adapter's legacy `api.mollie.nl` origin; pagination
uses `api.mollie.com`, as documented. Stripe optional fields include JSON null;
PayPal captures include `supplementary_data.related_ids.order_id`; Buckaroo status
responses are flat transaction objects, unlike webhook envelopes.

## Boundary and limits

`payment_psp_mock` intercepts `httpc:request/4` and `z_url_fetch:fetch/2,4`.
The real adapters, `z_fetch`, form encoding and JSON decoding run. Configuration
and Sovergn's token cache are isolated with meck. Both HTTP modules have **no
passthrough**. An unexpected request fails; each test checks its expected calls
were consumed. The mock checks exact methods, URLs, authentication, content types
and decoded request bodies against the independently authored fixture. Buckaroo
HMAC verification uses the documented algorithm, not an adapter helper. Negative
checks prove incorrect requests and credentials are rejected.

The endpoint matrix tests transport via TEST-only adapter exports, with valid
request examples. It does not claim to test every production request builder or
every field allowed by the PSP. Additional workflow tests exercise actual Mollie
pagination and cancellation, public Stripe/PayPal retrieval, Buckaroo status
conversion, Sovergn's bounded token-refresh retry, and PayPal webhook verification
and capture/order fetching through to `mod_payment:receive_payment/3`.

These are focused contract mocks, not full PSP emulators or live certification
tests. They cover the API operations we use, not unrelated service APIs. Shared
payment persistence, import policy and notification deduplication remain covered
by `payment_receive_db_tests`; that suite requires PostgreSQL.


## GitHub CI

Each repository's `.github/workflows/test.yml` runs on pushes to `main` and PRs
targeting `main`. Jobs use Erlang 28.5 and PostgreSQL 16, and check out Zotonic
`master` into `zotonic-master/`. The module under test is checked out at the event
revision (the merge revision for a PR) inside `apps_user/`.

PSP jobs also check out `zotonic/zotonic_mod_payment` at `main`; they do not check
out other PSPs. The `mod_payment` job checks out no PSPs. Land the shared test
runner in `mod_payment/main` before relying on the PSP workflows.

`test/run-ci.sh payment|mollie|stripe|paypal|buckaroo|sovergn` first starts Zotonic's
real test sandbox, installs `mod_payment` and the selected PSP, and runs that
module's unit tests. It uses an explicit test list because the stock
`bin/zotonic runtests` CLI discovers `apps/`, not `apps_user/`. A watchdog makes
sandbox startup/test failures fail the job instead of waiting indefinitely.
The HTTP contract tests run afterward in a separate VM to keep global meck
replacements away from live site workers.

For `payment`, the runner executes the real PostgreSQL persistence suite instead
of PSP contract tests. `PAYMENT_TEST_PORT` is mandatory in this mode so database
tests cannot silently skip. Optional `PAYMENT_TEST_HOST`, `PAYMENT_TEST_USER`,
`PAYMENT_TEST_PASSWORD` and `PAYMENT_TEST_DATABASE` select the disposable test
database; the suite creates and removes its own `payment_receive_test` schema.
The sandbox uses Zotonic's usual `ZOTONIC_DBHOST` / `ZOTONIC_DBPORT` configuration.
