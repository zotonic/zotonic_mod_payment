# PSP HTTP contract tests

Run from a compiled Zotonic umbrella checkout with the five PSP modules installed:

```sh
apps_user/zotonic_mod_payment/test/run-psp-contracts.sh
```

Pass a provider name (`mollie`, `stripe`, `paypal`, `buckaroo`, or `sovergn`) to
run only that provider's contract tests. Only that PSP and `mod_payment` need to
be checked out. With no argument, the runner tests all five providers.

The runner compiles the adapters and test files into a temporary directory and removes it
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
and Sovergn's token cache are isolated with meck. HTTP requests from the test
process have **no passthrough**: an unexpected request fails, even if the test
omits its request expectations. Each test checks its expected calls were consumed.
Calls from unrelated sandbox workers pass through to the original modules.
Context-based mocks apply only to the synthetic test contexts, preserving the
live site's configuration, cache, database pool and notifications. The mock checks exact methods, URLs, authentication, content types
and decoded request bodies against the independently authored fixture. Buckaroo
HMAC verification uses the documented algorithm, not an adapter helper. Negative
checks prove incorrect requests and credentials are rejected.

The endpoint matrix tests transport via explicitly documented adapter exports, with valid
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
real test sandbox, installs `mod_payment` and the selected PSP, and runs all of
that module's tests in the same sandbox VM, including its HTTP contract tests.
It discovers every `test/*_tests.erl` file because the stock `bin/zotonic runtests`
CLI discovers `apps/`, not `apps_user/`. A watchdog makes sandbox startup/test
failures fail the job instead of waiting indefinitely. A final database and
module-state check confirms that the sandbox still works after mock cleanup.

Tests live exclusively in `test/`, including the payment search-filter tests and
Sovergn diagnostic tests formerly guarded by `TEST` inside production files.
Tested helpers are exported unconditionally with comments explaining their use.
Neither test discovery nor adapter compilation depends on the `TEST` macro.

For `payment`, the runner executes the real PostgreSQL persistence suite in the sandbox instead
of PSP contract tests. `PAYMENT_TEST_PORT` is mandatory in this mode so database
tests cannot silently skip. Optional `PAYMENT_TEST_HOST`, `PAYMENT_TEST_USER`,
`PAYMENT_TEST_PASSWORD` and `PAYMENT_TEST_DATABASE` select the disposable test
database; the suite creates and removes its own `payment_receive_test` schema.
The sandbox uses Zotonic's usual `ZOTONIC_DBHOST` / `ZOTONIC_DBPORT` configuration.


## Discovery API fixtures

The `discovery-page` / `discovery-next` cases are based on these official schemas,
using synthetic identifiers and amounts (not captured production responses):

- [Mollie list payments](https://docs.mollie.com/reference/list-payments):
  `_embedded.payments`, HAL `next`, creation ordering and profile scope.
- [Stripe list Checkout Sessions](https://docs.stripe.com/api/checkout/sessions/list):
  `created` bounds, `starting_after`, `has_more`, and payment/subscription modes.
- [PayPal Transaction Search](https://developer.paypal.com/api/transaction-search/v1/search-get)
  and [event codes](https://developer.paypal.com/reports/reference/t-codes/):
  page/total_pages, signed gross amounts, T00 payments, T01 fees and S/P statuses.

Workflow tests exercise these responses through the real adapters and HTTP
boundary, including pagination, Mollie creation cutoff, non-payment exclusion,
PayPal's opt-in gate and canonical order identity. Core polling tests cover the
shared gates, failed imports/pages, retry boundaries and cursor loops; the real
PostgreSQL suite verifies one import/notification across repeated discovery.
