%% @doc Exercise real PSP serialization/decoding against documentation fixtures.
%% Site configuration and HTTP IO are mocked; z_fetch and adapter code are real.
-module(payment_psp_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

contracts_test_() ->
    {foreach, fun payment_psp_mock:start/0, fun(_) -> payment_psp_mock:stop() end,
        lists:append([provider_tests(P) || P <- providers()])}.

providers() ->
    case os:getenv("PAYMENT_TEST_PSP") of
        false -> [mollie, stripe, paypal, buckaroo, sovergn];
        "mollie" -> [mollie];
        "stripe" -> [stripe];
        "paypal" -> [paypal];
        "buckaroo" -> [buckaroo];
        "sovergn" -> [sovergn];
        Other -> error({unknown_psp, Other})
    end.

provider_tests(P) ->
    [{atom_to_list(P) ++ ": " ++ binary_to_list(maps:get(<<"name">>, C)),
        fun() -> contract(P, C) end} || C <- payment_psp_mock:fixtures(P)]
    ++ workflow_tests(P).

workflow_tests(mollie) ->
    [fun subscriptions/0, fun payments/0, fun cancellation/0, fun() -> errors(mollie) end];
workflow_tests(stripe) ->
    [fun stripe_fetch/0, fun reject/0, fun() -> errors(stripe) end];
workflow_tests(paypal) ->
    [fun paypal_fetch/0, fun paypal_webhook/0, fun paypal_verify/0,
     fun oauth_failure/0, fun() -> errors(paypal) end];
workflow_tests(buckaroo) ->
    [fun buckaroo_status/0];
workflow_tests(sovergn) ->
    [fun sovergn_retry/0].

contract(P, C) ->
    payment_psp_mock:expect(P, with_oauth(P, C)),
    ?assertEqual({ok, expected(P, C)}, call(P, C)),
    payment_psp_mock:done().

call(P, C) ->
    M = binary_to_existing_atom(maps:get(<<"method">>, C)),
    Path = maps:get(<<"path">>, C),
    Req = maps:get(<<"request">>, C),
    case P of
        mollie -> m_payment_mollie_api:api_call(M, Path, maps:to_list(Req), undefined);
        stripe -> m_payment_stripe_api:api_call(M, Path, maps:to_list(Req), undefined);
        paypal -> m_payment_paypal_api:api_call(M, Path, Req, undefined);
        buckaroo -> m_payment_buckaroo_api:api_call(M, Path, Req, en, undefined);
        sovergn -> payment_sovergn_api:api_json(M, Path, Req, scope(C),
            [{content_type, <<"application/json">>},
             {headers, [{<<"Idempotency-Key">>, <<"contract-test-key">>}]}], undefined)
    end.

with_oauth(P, C) when P =:= paypal; P =:= sovergn -> [oauth(P, C), C];
with_oauth(_, C) -> [C].

oauth(P, C) ->
    Path = case P of paypal -> <<"/v1/oauth2/token">>; sovergn -> <<"/oauth/token">> end,
    Req = #{<<"grant_type">> => <<"client_credentials">>},
    Response = #{<<"access_token">> => <<"token_mock">>, <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 300, <<"scope">> => case P of
            paypal -> <<"https://api.paypal.com/v1/payments/.*">>;
            sovergn -> scope(C)
        end},
    #{<<"name">> => <<"oauth">>, <<"method">> => <<"post">>, <<"path">> => Path,
        <<"request">> => case P of sovergn -> Req#{<<"scope">> => scope(C)}; paypal -> Req end,
        <<"status">> => 200, <<"response">> => Response}.

scope(#{<<"method">> := <<"get">>}) -> <<"payment_status:read">>;
scope(_) -> <<"checkout_sessions:create">>.

fixture(P, Name) ->
    [C] = [C || C <- payment_psp_mock:fixtures(P), maps:get(<<"name">>, C) =:= Name],
    C.

subscriptions() ->
    C = fixture(mollie, <<"list-subscriptions">>),
    N = fixture(mollie, <<"list-subscriptions-next">>),
    payment_psp_mock:expect(mollie, [C, N]),
    #{<<"_embedded">> := #{<<"subscriptions">> := Subs}} = expected(mollie, C),
    ?assertEqual({ok, Subs}, m_payment_mollie_api:mollie_list_subscriptions(<<"cst_mock123">>, undefined)),
    payment_psp_mock:done().

payments() ->
    C = fixture(mollie, <<"list-payments">>),
    N = fixture(mollie, <<"list-payments-next">>),
    payment_psp_mock:expect(mollie, [C, N]),
    #{<<"_embedded">> := #{<<"payments">> := Payments}} = expected(mollie, C),
    ?assertEqual({ok, Payments}, m_payment_mollie_api:fetch_payments_since_loop(
        <<"2026-09-01T00:00:00+00:00">>, undefined, [], undefined)),
    payment_psp_mock:done().

cancellation() ->
    payment_psp_mock:expect(mollie, [fixture(mollie, <<"cancel-subscription">>)]),
    ?assertEqual(ok, m_payment_mollie_api:cancel_subscription(
        {mollie_subscription, <<"cst_mock123">>, <<"sub_mock123">>}, undefined)),
    payment_psp_mock:done().

stripe_fetch() ->
    C = fixture(stripe, <<"retrieve">>),
    payment_psp_mock:expect(stripe, [C]),
    ?assertEqual({ok, expected(stripe, C)},
        m_payment_stripe_api:fetch_session(<<"cs_test_mock123">>, undefined)),
    payment_psp_mock:done().

paypal_fetch() ->
    C = fixture(paypal, <<"get">>),
    payment_psp_mock:expect(paypal, with_oauth(paypal, C)),
    ?assertEqual({ok, expected(paypal, C)},
        m_payment_paypal_api:fetch_order(<<"ORDERMOCK123">>, undefined)),
    payment_psp_mock:done().

buckaroo_status() ->
    C = fixture(buckaroo, <<"status">>),
    payment_psp_mock:expect(buckaroo, [C]),
    ?assertEqual({ok, {190, {{2026,10,1},{12,0,0}}}},
        m_payment_buckaroo_api:transaction_status(<<"0123456789abcdef0123456789abcdef">>,
            #context{language = [en]})),
    payment_psp_mock:done().

sovergn_retry() ->
    C = fixture(sovergn, <<"checkout">>),
    Unauthorized = C#{<<"status">> => 401, <<"response">> => #{<<"error">> => <<"unauthorized">>,
        <<"message">> => <<"Authentication required">>}},
    payment_psp_mock:expect(sovergn, [oauth(sovergn, C), Unauthorized, oauth(sovergn, C), C]),
    ?assertEqual({ok, expected(sovergn, C)}, call(sovergn, C)),
    payment_psp_mock:done(),
    %% A second 401 must be returned, not retried forever.
    payment_psp_mock:expect(sovergn, [oauth(sovergn, C), Unauthorized, oauth(sovergn, C), Unauthorized]),
    ?assertMatch({error, {401, _, _, _, _}}, call(sovergn, C)),
    payment_psp_mock:done().

errors(Provider) ->
    lists:foreach(fun({P, Name, Body}) ->
        C = (fixture(P, Name))#{<<"status">> => 404, <<"response">> => Body},
        payment_psp_mock:expect(P, with_oauth(P, C)),
        ?assertEqual({error, 404}, call(P, C)),
        payment_psp_mock:done()
    end, [E || {P, _, _} = E <- [
        {mollie, <<"get-payment">>, #{<<"status">> => 404, <<"title">> => <<"Not Found">>, <<"detail">> => <<"No payment exists with this ID.">>}},
        {stripe, <<"retrieve">>, #{<<"error">> => #{<<"type">> => <<"invalid_request_error">>, <<"code">> => <<"resource_missing">>, <<"message">> => <<"No such checkout.session">>}}},
        {paypal, <<"get">>, #{<<"name">> => <<"RESOURCE_NOT_FOUND">>, <<"message">> => <<"The specified resource does not exist.">>, <<"debug_id">> => <<"mock">>}}
    ], P =:= Provider]).

reject() ->
    payment_psp_mock:expect(stripe, []),
    ?assertError({unexpected_psp_request, get, <<"https://api.stripe.com/unused">>, []},
        payment_psp_mock:request(get, <<"https://api.stripe.com/unused">>, <<>>, <<"Bearer sk_test_mock">>, <<>>)),
    C = fixture(stripe, <<"create">>),
    payment_psp_mock:expect(stripe, [C]),
    ?assertException(error, {assertEqual, _}, payment_psp_mock:request(post,
        <<"https://api.stripe.com/v1/checkout/sessions">>, <<"mode=subscription">>,
        <<"Bearer sk_test_mock">>, <<"application/x-www-form-urlencoded">>)),
    %% Rejection must not consume the pending request.
    ?assertException(error, {assertEqual, _}, payment_psp_mock:request(post,
        <<"https://api.stripe.com/v1/checkout/sessions">>, <<>>, <<"Bearer wrong">>, <<>>)).

%% Stripe uses jsx (null); the other adapters use Zotonic JSON (undefined).
expected(stripe, C) -> maps:get(<<"response">>, C);
expected(_, C) -> z_json:decode(jsx:encode(maps:get(<<"response">>, C))).


paypal_webhook() ->
    Capture = fixture(paypal, <<"get-capture">>),
    Order = fixture(paypal, <<"get">>),
    payment_psp_mock:expect(paypal, with_oauth(paypal, Capture) ++ with_oauth(paypal, Order)),
    meck:new(mod_payment, [no_link]),
    try
        meck:expect(mod_payment, receive_payment, fun(mod_payment_paypal, Data, _) ->
            ?assertEqual(<<"ORDERMOCK123">>, maps:get(psp_external_id, Data)),
            ?assertEqual(<<"default">>, maps:get(payment_nr, Data)),
            ?assertEqual(paid, maps:get(status, Data)),
            ?assertEqual(10.0, maps:get(amount, Data)),
            ?assertEqual(<<"EUR">>, maps:get(currency, Data)),
            ?assertEqual(<<"EVENTMOCK">>, maps:get(event_id, Data)),
            {ok, 123}
        end),
        %% The notification is only a hint: status and amount come from API GETs.
        Event = #{<<"id">> => <<"EVENTMOCK">>,
            <<"event_type">> => <<"PAYMENT.CAPTURE.COMPLETED">>,
            <<"resource">> => #{<<"id">> => <<"CAPTUREMOCK123">>}},
        ?assertEqual(ok, m_payment_paypal_api:webhook_event(Event, #context{})),
        ?assertEqual(1, meck:num_calls(mod_payment, receive_payment, '_')),
        payment_psp_mock:done()
    after
        meck:unload(mod_payment)
    end.

paypal_verify() ->
    C = fixture(paypal, <<"verify">>),
    Payload = maps:get(<<"request">>, C),
    Headers = #{<<"paypal-auth-algo">> => <<"auth_algo">>,
        <<"paypal-cert-url">> => <<"cert_url">>,
        <<"paypal-transmission-id">> => <<"transmission_id">>,
        <<"paypal-transmission-sig">> => <<"transmission_sig">>,
        <<"paypal-transmission-time">> => <<"transmission_time">>},
    meck:new(z_context, [passthrough, no_link]),
    try
        meck:expect(z_context, get_req_header, fun(H, _) -> maps:get(maps:get(H, Headers), Payload) end),
        Body = jsx:encode(maps:get(<<"webhook_event">>, Payload)),
        payment_psp_mock:expect(paypal, with_oauth(paypal, C)),
        ?assert(m_payment_paypal_api:is_valid_webhook_signature(Body, undefined)),
        payment_psp_mock:done(),
        Failure = C#{<<"response">> => #{<<"verification_status">> => <<"FAILURE">>}},
        payment_psp_mock:expect(paypal, with_oauth(paypal, Failure)),
        ?assertNot(m_payment_paypal_api:is_valid_webhook_signature(Body, undefined)),
        payment_psp_mock:done()
    after
        meck:unload(z_context)
    end.

oauth_failure() ->
    C = fixture(paypal, <<"get">>),
    Token = (oauth(paypal, C))#{<<"status">> => 401,
        <<"response">> => #{<<"error">> => <<"invalid_client">>,
            <<"error_description">> => <<"Client Authentication failed">>}},
    payment_psp_mock:expect(paypal, [Token]),
    ?assertEqual({error, 401}, call(paypal, C)),
    payment_psp_mock:done().
