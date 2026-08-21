%% @copyright 2026 Driebit BV
%% @doc Show a payment currency using its preferred short form.
%% @end

%% Copyright 2026 Driebit BV
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(filter_payment_currency_symbol).
-moduledoc("
Show a payment currency using its preferred symbol or short form.

For example, `EUR` is shown as `€`:

```django
{{ currency|payment_currency_symbol }}
```

Unknown currency codes are returned unchanged.
").

-export([payment_currency_symbol/2]).

-spec payment_currency_symbol(term(), z:context()) -> binary().
payment_currency_symbol(undefined, _Context) -> <<>>;
payment_currency_symbol(<<"AUD">>, _Context) -> <<"A$">>;
payment_currency_symbol(<<"BRL">>, _Context) -> <<"R$">>;
payment_currency_symbol(<<"CAD">>, _Context) -> <<"C$">>;
payment_currency_symbol(<<"CNY">>, _Context) -> <<"CN¥"/utf8>>;
payment_currency_symbol(<<"CZK">>, _Context) -> <<"Kč"/utf8>>;
payment_currency_symbol(<<"DKK">>, _Context) -> <<"DKK">>;
payment_currency_symbol(<<"EUR">>, _Context) -> <<"€"/utf8>>;
payment_currency_symbol(<<"HKD">>, _Context) -> <<"HK$">>;
payment_currency_symbol(<<"HUF">>, _Context) -> <<"Ft">>;
payment_currency_symbol(<<"ILS">>, _Context) -> <<"₪"/utf8>>;
payment_currency_symbol(<<"JPY">>, _Context) -> <<"¥"/utf8>>;
payment_currency_symbol(<<"MYR">>, _Context) -> <<"RM">>;
payment_currency_symbol(<<"MXN">>, _Context) -> <<"MX$">>;
payment_currency_symbol(<<"TWD">>, _Context) -> <<"NT$">>;
payment_currency_symbol(<<"NZD">>, _Context) -> <<"NZ$">>;
payment_currency_symbol(<<"NOK">>, _Context) -> <<"NOK">>;
payment_currency_symbol(<<"PHP">>, _Context) -> <<"₱"/utf8>>;
payment_currency_symbol(<<"PLN">>, _Context) -> <<"zł"/utf8>>;
payment_currency_symbol(<<"GBP">>, _Context) -> <<"£"/utf8>>;
payment_currency_symbol(<<"SGD">>, _Context) -> <<"S$">>;
payment_currency_symbol(<<"SEK">>, _Context) -> <<"SEK">>;
payment_currency_symbol(<<"CHF">>, _Context) -> <<"CHF">>;
payment_currency_symbol(<<"THB">>, _Context) -> <<"฿"/utf8>>;
payment_currency_symbol(<<"USD">>, _Context) -> <<"$">>;
payment_currency_symbol(Currency, _Context) when is_binary(Currency) -> Currency;
payment_currency_symbol(Currency, Context) ->
    payment_currency_symbol(z_convert:to_binary(Currency), Context).
