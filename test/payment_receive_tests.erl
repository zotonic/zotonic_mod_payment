-module(payment_receive_tests).
-include_lib("eunit/include/eunit.hrl").

update() ->
    #{psp_external_id => <<"psp-1">>, status => paid,
      status_date => {{2026, 10, 29}, {10, 0, 0}}, amount => 12.34, currency => <<"EUR">>}.

validation_test() ->
    D = update(),
    ?assertEqual(ok, payment_receive:validate(D)),
    ?assertEqual(ok, payment_receive:validate(D#{currency => <<"KRW">>, amount => 0})),
    lists:foreach(fun(Bad) ->
        ?assertEqual({error, payment_data}, payment_receive:validate(Bad))
    end, [D#{status => <<"paid">>}, D#{status => arbitrary}, D#{amount => -1},
          D#{currency => <<"eur">>}, D#{psp_external_id => <<>>},
          D#{psp_external_id => binary:copy(<<"x">>, 129)},
          D#{status_date => {{2026, 2, 30}, {0, 0, 0}}}, D#{contact => null},
          D#{event_id => binary:copy(<<"x">>, 129)}]).

identity_test() ->
    D = update(),
    P = #{<<"psp_module">> => mod_payment_stripe, <<"psp_external_id">> => <<"psp-1">>,
          <<"amount">> => 12.34, <<"currency">> => <<"EUR">>},
    ?assert(payment_receive:matches(mod_payment_stripe, D, P)),
    ?assertNot(payment_receive:matches(mod_payment_paypal, D, P)),
    ?assertNot(payment_receive:matches(mod_payment_stripe, D#{psp_external_id => <<"other">>}, P)),
    ?assertNot(payment_receive:matches(mod_payment_stripe, D#{amount => 10}, P)),
    ?assertNot(payment_receive:matches(mod_payment_stripe, D#{currency => <<"USD">>}, P)),
    ?assertNot(payment_receive:matches(mod_payment_stripe, D, P#{<<"psp_external_id">> => undefined})).
