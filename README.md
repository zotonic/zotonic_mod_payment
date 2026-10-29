Payments module for Zotonic
===========================

Base module for handling payments. Uses Payment Service Provider (PSP) modules for interfacing with
the payment providers.


Payment form
------------

To add a payment form to your website, make a form with the following postback:

```django
{% wire id="mypayment"
        type="submit"
        postback={payment key="web-donation"}
        delegate=`mod_payment
%}
<form id="mypayment" method="post" action="postback">
    ...
</form>
```

Optional arguments of the payment postback:

 * `key` - an identification of the payment type, also shown on the overview in the admin
 * `amount` - the amount to be paid, overrules the `amount` form field
 * `currency` - the currency for the amount, overrules `currency` form field (defaults to EUR)
 * `is_recurring_start` - if the payment is recurring, overrules `is_recurring_start` form field (defaults to false)
 * `user_id` - the id of the user the payment is for (defaults to the current user)
 * `description` - the description for the ordered goods/services (HTML)
 * `default_description` - used as the description if both `description` and `q.description` are empty
 * All other arguments are saved as additional properties, which can be overruled with form fields

Required fields for the payment form:

 * `amount` - if there is no amount in the postback, an integer of floating point number
 * `email` - email address (defaults to current user's)
 * `name_surname` - surname, required if there is no user_id (defaults to current user's)

Optional fields for the payment form:

 * `currency` - if there is no currency in the postback, the used currency, defaults to `EUR`
 * `is_recurring_start` - flag if the payment should be a recurring payment, defaults to `false` (see section below)
 * `name_first` - first name (defaults to current user's)
 * `phone` - phone number (defaults to current user's)
 * `address_street_1` - First line of address (defaults to current user's)
 * `address_street_2` - Second line of address (defaults to current user's)
 * `address_city` - City of address (defaults to current user's)
 * `address_state` - State of address (defaults to current user's)
 * `address_postcode` - Postcode of address (defaults to current user's)
 * `address_country` - Country of address, should be two letter ISO code (defaults to current user's)
 * `description` - Description of what has been ordered, could also be a referral identifier
 * `note` - Optional user defined note
 * `reference` - Optional user defined reference number, appended to the description.


Other form fields will be stored as-is in the props of the payment record.


Supported currencies
--------------------

The module currently accepts these 24 currencies:

* `AUD`, `BRL`, `CAD`, `CNY`, `CZK`, `DKK`, `EUR`, `HKD`
* `HUF`, `ILS`, `JPY`, `MYR`, `MXN`, `TWD`, `NZD`, `NOK`
* `PHP`, `PLN`, `GBP`, `SGD`, `SEK`, `CHF`, `THB`, `USD`

This is the set supported by PayPal and is also supported by Stripe. Stripe
supports additional currencies, but they are not currently accepted by
`mod_payment`.

Amounts in `HUF`, `JPY`, and `TWD` must be whole numbers. Amounts in the other
currencies can have at most two decimal places. Availability can be further
restricted by the payment provider, payment method, merchant country, and
merchant account configuration.


Subscriptions / Recurring Payments
----------------------------------

Subscriptions are recurring payments.
All subscriptions *MUST* have an user_id attached.

The period of the recurring payment (monthly / yearly) is user-defined and should be explained
to the user in explanatory texts. The used PSP should know about this period as well.

The module `mod_payment` only initiates the first payment. Any subsequent payments should be initiated
by the used PSP.

Any user can have at most one (1) subscription.

The subscription can be canceled with the following postback:

```django
{% wire id='cancel-subscription-button'
        postback={cancel_recurring}
        delegate=`mod_payment`
%}
```


Payment Service Provider Modules
--------------------------------

This module provides the central payment administration. Interfacing with Payment Service Providers (PSP)
is done using PSP specific modules.

PSP specific modules are being implemented for Stripe, Mollie, Buckaroo and PayPal:

 * https://github.com/zotonic/zotonic_mod_payment_stripe
 * https://github.com/zotonic/zotonic_mod_payment_mollie
 * https://github.com/zotonic/zotonic_mod_payment_buckaroo
 * https://github.com/zotonic/zotonic_mod_payment_paypal

Please check the modules for PSP specific configurations.


Configuration
-------------

The admin page **System → Payment configuration** manages general settings and
all enabled PSPs. Viewing and saving require `use.mod_admin_config`. Each PSP
contributes `_admin_payment_config_psp.tpl`, included using `{% all include %}`.
Sections use the shared `-mod_config` schema and save independently. Credentials use password inputs with the standard show-password control.
Emptying a field clears its value when the section is saved. Internal synchronization state is read-only.

There are several configuration keys:

 * `mod_payment.description` default description for the payments. Can be overruled in the payment form or postback.
 * `mod_payment.amount` default amount for payments, must be an integer value.
 * `mod_payment.currency` default currency for payments, defaults to `EUR`.
 * `mod_payment.email_paid` one of more email addresses to send an email for every payment that is set to `paid`.
 * `mod_payment.delete_after_days` if set and greater than 1, then payments not modified for this number of days
   are deleted. All payment data and payment logs are deleted. This is useful to retain as little personal information
   for as short as needed (GDPR compliance).

Check also the configuration keys for the PSP specific modules.


Receiving external payments
---------------------------

By default, verified webhooks for unknown payments are rejected. To import them,
set the site configuration `mod_payment.accept_unknown_payments` to `true`.
Known payments continue to update regardless of this setting.

The shared internal API is:

```erlang
mod_payment:receive_payment(PspModule, #{
    psp_external_id => <<"provider-payment-id">>,
    status => paid,
    status_date => {{2026, 10, 29}, {10, 0, 0}},
    amount => 12.34,
    currency => <<"EUR">>,
    event_id => <<"provider-event-id">>,
    psp_data => ProviderData
}, Context).
```

It returns `{ok, Payment}` or `{error, Reason}`. Call it only after verifying the
PSP signature or retrieving the payment with the configured PSP credentials.
It is not a browser/model endpoint and must not be used to import from redirects.

Required fields are `psp_external_id`, a normalized status atom, and a UTC
`status_date`. Import also requires a nonnegative numeric `amount` in major units
and an uppercase three-letter `currency`; these are not limited to currencies
supported by the outgoing checkout form. For existing payments, supplied amount
and currency must match the stored values. Optional fields are `payment_nr`
(local correlation), `description`, `contact` (a map with binary contact-field
keys), `event_id`, and `psp_data`. PSP adapters own currency-unit conversion and
mapping provider statuses to the existing local status vocabulary.

Lookup, import policy, contact sanitization, event logging, and status updates
are shared. A local reference cannot reassign an existing payment to another PSP
or external ID. Initialization still in progress results in an error for retry.
Imports receive a random local payment number, no local user or business key,
and `is_external = true` in their properties and the returned payment map.
Contact details are optional. Imports never initiate a checkout or subscription.

The initial import and subsequent status changes emit `#payment_status{}` with
`is_external = true`; existing observers must be recompiled with the updated
header. The initial notification has `previous_status = undefined`. Paid imports
also use the existing `mod_payment.email_paid` recipients. Retries do not repeat
status notifications. Event IDs, when supplied, deduplicate successful deliveries
per payment. Older statuses and open/pending updates for closed payments cannot
reopen a payment. Notifications are emitted after the receive transaction commits;
this is not a durable notification outbox.

Schema version 10 adds a unique `(psp_module, psp_external_id)` index for nonempty
PSP IDs. The same index is created on fresh installations and upgrades. Existing
duplicate identities must be reconciled before upgrading; the migration fails
rather than deleting or merging payment records. Per-identity transaction locks
serialize concurrent webhook imports and updates.

Implemented webhook adapters:

* Buckaroo: verified incoming debit transaction pushes, including the generic
  `/buckaroo/webhook` route. Existing `/buckaroo/webhook/<payment_nr>` URLs remain
  valid. Credit transactions are not imported as incoming payments.
* Stripe: the existing Checkout Session event family, including sessions without
  Zotonic metadata. PaymentIntent, subscription, and charge event families are
  outside this change.
* PayPal: the existing order/capture event families. Orders are the canonical
  identity for related captures. Standalone captures use a `capture:` ID prefix.
  Refund events retrieve their parent capture so refund amounts are never imported
  as new payments. Partial refunds retain the existing `paid` status convention.

Mollie's numbered webhook and recurring-payment handling, and Sovergn's restricted
integration, remain provider-specific. Scheduled discovery is described below;
it does not perform a full historical backfill.

The pure tests are `payment_receive_tests` and the PSP API test modules. The
optional `payment_receive_db_tests` runs when `PAYMENT_TEST_PORT` is set. It expects
an isolated PostgreSQL instance on localhost, database `postgres`, user
`payment_test`, and trust authentication. It creates and removes only the
`payment_receive_test` schema, testing imports, policy, identity checks, stale
updates, concurrent retries, and fresh/upgrade schema behavior.


### Offline PSP contract tests

The [PSP contract suite](test/psp_contracts/README.md) mocks only the HTTP APIs
used by Mollie, Stripe, PayPal, Buckaroo and Sovergn, using documented PSP
requests and responses. Run `apps_user/zotonic_mod_payment/test/run-psp-contracts.sh`
from the compiled Zotonic umbrella checkout. No live PSP credentials or database
are required.


### Discovering missed payments

With `mod_payment.accept_unknown_payments = true`, `mod_payment` starts a unique
background discovery job on `tick_4h` (requires `mod_cron`). Set
`mod_payment.disable_unknown_payment_polling = true` to disable discovery while
continuing to accept verified webhooks. Both settings are checked again between
pages and before importing; known-payment status synchronization is unchanged.

The first run looks back 24 hours. Subsequent runs resume from a per-PSP checkpoint
and overlap the previous day to cover reporting delays. If `delete_after_days`
is configured (at least 2 days), the polling window is clamped to that retention
period and individual results older than the cutoff are skipped, using the PSP's
original creation/initiation date rather than a fresh status observation. Mollie's
legacy recurring-payment poll uses the same policy for unknown payments. This
prevents polling from recreating expired payments, without changing webhook or
known-payment updates. With retention unset, there is no extra age restriction. Long outages are caught
up in seven-day steps on successive ticks. This is ongoing discovery, not a full
historical account import. Checkpoints live in the properties of
`mod_payment.payment_poll`; do not change them during a running job. Changing PSP
accounts or live/test credentials requires resetting that PSP's checkpoint.

Each page is imported through `mod_payment:receive_payment/3`; already-known PSP
identities are skipped. A failed request, invalid payment, database error or
pagination loop leaves the checkpoint unchanged for retry on the next tick.
Successful imports before a failure are safe to encounter again. The 1,000-page
safety limit also fails without advancing the checkpoint.

Supported discovery APIs:

- **Mollie:** payments in the profile selected by its API key. No local webhook
  URL or local reference is required. Pages are ordered by creation time.
- **Stripe:** payment-mode Checkout Sessions, matching this module's payment
  identity. Direct Charges/PaymentIntents and subscription sessions are outside
  this adapter's scope.
- **PayPal:** additionally requires `mod_payment_paypal.enable_reporting_polling = true`
  (default `false`) and the Transaction Search permission
  `https://uri.paypal.com/services/reporting/search/read`. Incoming positive
  payment transactions are selected; fees, withdrawals, currency conversions and
  refund records are not imported as new payments. Order references resolve to
  the existing order identity; standalone/legacy payments use capture IDs.
  Pending legacy imports also use reporting for their existing status sync.
  Reporting can lag by three hours. Missing permissions or inaccessible order
  details fail the poll and preserve its checkpoint; they are not silently skipped.
- **Buckaroo:** the checkout gateway has no documented discovery/list endpoint;
  it continues to use webhooks and known-transaction status queries.
- **Sovergn:** discovery is deliberately not enabled.

To add a provider, observe `#payment_psp_poll{}`. An untargeted `map` notification
asks for supported provider module atoms. A targeted `first` notification supplies
UTC `since`/`until` boundaries and an opaque page cursor. Include the original PSP
creation datetime as `created_at` in each payment map for retention checks. Return
`{ok, NormalizedPayments, NextCursor}` (`undefined` at the end), or `{error, Reason}`.
The provider retrieves authenticated API data and normalizes it; shared code owns
configuration gates, pagination, checkpoints, deduplication and imports.
