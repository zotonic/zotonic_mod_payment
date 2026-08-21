-module(mod_payment_tests).
-moduledoc("
EUnit tests for payment currency and amount validation.
").

-include_lib("eunit/include/eunit.hrl").

supported_currencies_test() ->
    Currencies = [
        <<"AUD">>,
        <<"BRL">>,
        <<"CAD">>,
        <<"CNY">>,
        <<"CZK">>,
        <<"DKK">>,
        <<"EUR">>,
        <<"HKD">>,
        <<"HUF">>,
        <<"ILS">>,
        <<"JPY">>,
        <<"MYR">>,
        <<"MXN">>,
        <<"TWD">>,
        <<"NZD">>,
        <<"NOK">>,
        <<"PHP">>,
        <<"PLN">>,
        <<"GBP">>,
        <<"SGD">>,
        <<"SEK">>,
        <<"CHF">>,
        <<"THB">>,
        <<"USD">>
    ],
    ?assertEqual(24, length(Currencies)),
    ?assert(lists:all(fun mod_payment:is_valid_currency/1, Currencies)),
    ?assertNot(mod_payment:is_valid_currency(<<"RUB">>)),
    ?assertNot(mod_payment:is_valid_currency(<<"eur">>)),
    ?assertNot(mod_payment:is_valid_currency(<<"EURO">>)).

payment_amount_currency_test() ->
    ?assert(mod_payment:is_valid_payment_args(12.34, <<"EUR">>)),
    ?assertNot(mod_payment:is_valid_payment_args(12.345, <<"EUR">>)),
    ?assert(mod_payment:is_valid_payment_args(1234, <<"JPY">>)),
    ?assertNot(mod_payment:is_valid_payment_args(1234.5, <<"JPY">>)),
    ?assert(mod_payment:is_valid_payment_args(1234, <<"HUF">>)),
    ?assertNot(mod_payment:is_valid_payment_args(1234.5, <<"HUF">>)),
    ?assert(mod_payment:is_valid_payment_args(1234, <<"TWD">>)),
    ?assertNot(mod_payment:is_valid_payment_args(1234.5, <<"TWD">>)).

payment_currency_symbol_test() ->
    Symbols = [
        {<<"AUD">>, <<"A$">>},
        {<<"BRL">>, <<"R$">>},
        {<<"CAD">>, <<"C$">>},
        {<<"CNY">>, <<"CN¥"/utf8>>},
        {<<"CZK">>, <<"Kč"/utf8>>},
        {<<"DKK">>, <<"DKK">>},
        {<<"EUR">>, <<"€"/utf8>>},
        {<<"HKD">>, <<"HK$">>},
        {<<"HUF">>, <<"Ft">>},
        {<<"ILS">>, <<"₪"/utf8>>},
        {<<"JPY">>, <<"¥"/utf8>>},
        {<<"MYR">>, <<"RM">>},
        {<<"MXN">>, <<"MX$">>},
        {<<"TWD">>, <<"NT$">>},
        {<<"NZD">>, <<"NZ$">>},
        {<<"NOK">>, <<"NOK">>},
        {<<"PHP">>, <<"₱"/utf8>>},
        {<<"PLN">>, <<"zł"/utf8>>},
        {<<"GBP">>, <<"£"/utf8>>},
        {<<"SGD">>, <<"S$">>},
        {<<"SEK">>, <<"SEK">>},
        {<<"CHF">>, <<"CHF">>},
        {<<"THB">>, <<"฿"/utf8>>},
        {<<"USD">>, <<"$">>}
    ],
    lists:foreach(
        fun({Currency, Symbol}) ->
            ?assertEqual(
                Symbol,
                filter_payment_currency_symbol:payment_currency_symbol(Currency, undefined))
        end,
        Symbols),
    ?assertEqual(
        <<"XXX">>,
        filter_payment_currency_symbol:payment_currency_symbol(<<"XXX">>, undefined)).
