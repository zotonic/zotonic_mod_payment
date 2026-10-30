%% @copyright 2026 Marc Worrell
%% @doc Exercise real PSP serialization/decoding against documentation fixtures.
%% Site configuration and HTTP IO are mocked; z_fetch and adapter code are real.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(payment_psp_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/payment.hrl").

contracts_test_() ->
    {foreach, fun payment_psp_mock:start/0, fun(_) -> payment_psp_mock:stop() end,
        [scope_requests(T) || T <- lists:append([provider_tests(P) || P <- providers()])]}.

%% EUnit runs each body in its own process. Mark it before any adapter call,
%% so even a missing expect/2 fails closed instead of reaching the PSP.
scope_requests({Description, Fun}) -> {Description, scope_requests(Fun)};
scope_requests(Fun) ->
    fun() ->
        put(psp_requests, []),
        try Fun() after erase(psp_requests) end
    end.

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
    ++ workflow_tests(P) ++ polling_tests(P).

workflow_tests(mollie) ->
    [fun subscriptions/0, fun payments/0, fun mollie_retention/0, fun cancellation/0, fun() -> errors(mollie) end];
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
        mollie -> psp_call(m_payment_mollie_api, api_call, [M, Path, maps:to_list(Req), #context{}]);
        stripe -> psp_call(m_payment_stripe_api, api_call, [M, Path, maps:to_list(Req), #context{}]);
        paypal -> psp_call(m_payment_paypal_api, api_call, [M, Path, Req, #context{}]);
        buckaroo -> psp_call(m_payment_buckaroo_api, api_call, [M, Path, Req, en, #context{}]);
        sovergn -> psp_call(payment_sovergn_api, api_json, [M, Path, Req, scope(C),
            [{content_type, <<"application/json">>},
             {headers, [{<<"Idempotency-Key">>, <<"contract-test-key">>}]}], #context{}])
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
    ?assertEqual({ok, Subs}, psp_call(m_payment_mollie_api, mollie_list_subscriptions, [<<"cst_mock123">>, #context{}])),
    payment_psp_mock:done().

payments() ->
    C = fixture(mollie, <<"list-payments">>),
    N = fixture(mollie, <<"list-payments-next">>),
    payment_psp_mock:expect(mollie, [C, N]),
    #{<<"_embedded">> := #{<<"payments">> := Payments}} = expected(mollie, C),
    ?assertEqual({ok, Payments}, psp_call(m_payment_mollie_api, fetch_payments_since_loop, [
        <<"2026-09-01T00:00:00+00:00">>, undefined, [], #context{}])),
    payment_psp_mock:done().

cancellation() ->
    payment_psp_mock:expect(mollie, [fixture(mollie, <<"cancel-subscription">>)]),
    ?assertEqual(ok, psp_call(m_payment_mollie_api, cancel_subscription, [
        {mollie_subscription, <<"cst_mock123">>, <<"sub_mock123">>}, #context{}])),
    payment_psp_mock:done().

stripe_fetch() ->
    C = fixture(stripe, <<"retrieve">>),
    payment_psp_mock:expect(stripe, [C]),
    ?assertEqual({ok, expected(stripe, C)},
        psp_call(m_payment_stripe_api, fetch_session, [<<"cs_test_mock123">>, #context{}])),
    payment_psp_mock:done().

paypal_fetch() ->
    C = fixture(paypal, <<"get">>),
    payment_psp_mock:expect(paypal, with_oauth(paypal, C)),
    ?assertEqual({ok, expected(paypal, C)},
        psp_call(m_payment_paypal_api, fetch_order, [<<"ORDERMOCK123">>, #context{}])),
    payment_psp_mock:done().

buckaroo_status() ->
    C = fixture(buckaroo, <<"status">>),
    payment_psp_mock:expect(buckaroo, [C]),
    ?assertEqual({ok, {190, {{2026,10,1},{12,0,0}}}},
        psp_call(m_payment_buckaroo_api, transaction_status, [<<"0123456789abcdef0123456789abcdef">>,
            #context{language = [en]}])),
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
    meck:new(mod_payment, [passthrough, no_link]),
    try
        payment_test_mock:expect(mod_payment, receive_payment, psp, fun(mod_payment_paypal, Data, _) ->
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
        ?assertEqual(ok, psp_call(m_payment_paypal_api, webhook_event, [Event, #context{}])),
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
        payment_test_mock:expect(z_context, get_req_header, psp, fun(H, _) -> maps:get(maps:get(H, Headers), Payload) end),
        Body = jsx:encode(maps:get(<<"webhook_event">>, Payload)),
        payment_psp_mock:expect(paypal, with_oauth(paypal, C)),
        ?assert(psp_call(m_payment_paypal_api, is_valid_webhook_signature, [Body, #context{}])),
        payment_psp_mock:done(),
        Failure = C#{<<"response">> => #{<<"verification_status">> => <<"FAILURE">>}},
        payment_psp_mock:expect(paypal, with_oauth(paypal, Failure)),
        ?assertNot(psp_call(m_payment_paypal_api, is_valid_webhook_signature, [Body, #context{}])),
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

%% List responses and pagination fields come from the linked PSP docs fixtures.
%% These calls exercise the real HTTP serialization and response normalization.
polling_tests(P) when P =:= mollie; P =:= stripe; P =:= paypal ->
    [{"discovery pages: " ++ atom_to_list(P), fun() -> discovery(P) end}]
    ++ [{"late payment discovery: " ++ atom_to_list(P), fun() -> late_discovery(P) end}]
    ++ case P of paypal -> [fun reporting_disabled/0, fun reporting_order/0,
        fun reporting_permission_error/0, fun reporting_capture/0, fun reporting_pending_sync/0]; _ -> [] end;
polling_tests(_) -> [].

poll_request() ->
    #payment_psp_poll{since = {{2026,10,1},{0,0,0}}, until = {{2026,10,2},{0,0,0}}}.

poll(mollie, R) -> psp_call(mod_payment_mollie, observe_payment_psp_poll, [R#payment_psp_poll{psp_module = mod_payment_mollie}, #context{}]);
poll(stripe, R) -> psp_call(mod_payment_stripe, observe_payment_psp_poll, [R#payment_psp_poll{psp_module = mod_payment_stripe}, #context{}]);
poll(paypal, R) -> psp_call(mod_payment_paypal, observe_payment_psp_poll, [R#payment_psp_poll{psp_module = mod_payment_paypal}, #context{}]).

discovery(P) ->
    A = fixture(P, <<"discovery-page">>),
    B = fixture(P, <<"discovery-next">>),
    Extra = case P of paypal -> []; _ -> [fixture(P, <<"discovery-events-empty">>)] end,
    payment_psp_mock:expect(P, with_oauth(P, A) ++ with_oauth(P, B) ++ Extra),
    {ok, [Data], Next} = poll(P, poll_request()),
    ?assertEqual(ok, payment_receive:validate(Data)),
    ?assertEqual({{2026,10,1},{12,0,0}}, maps:get(created_at, Data)),
    ?assertEqual(paid, maps:get(status, Data)),
    ?assertEqual(12.34, maps:get(amount, Data)),
    ?assertEqual(<<"EUR">>, maps:get(currency, Data)),
    ?assertEqual(undefined, maps:get(payment_nr, Data, undefined)),
    {ok, Tail, EventCursor} = poll(P, (poll_request())#payment_psp_poll{cursor = Next}),
    case P of
        paypal ->
            [Pending] = Tail,
            ?assertEqual(pending, maps:get(status, Pending)),
            ?assertEqual(<<"capture:9AB12345CD678901E">>, maps:get(psp_external_id, Data));
        _ ->
            ?assertEqual([], Tail),
            ?assertMatch({ok, [], undefined}, poll(P, (poll_request())#payment_psp_poll{cursor = EventCursor}))
    end,
    payment_psp_mock:done().

reporting_disabled() ->
    payment_test_mock:expect(m_config, get_boolean, psp, fun(_, _, _) -> false end),
    ?assertEqual(undefined, psp_call(mod_payment_paypal, observe_payment_psp_poll, [#payment_psp_poll{}, #context{}])),
    ?assertEqual({error, reporting_disabled}, poll(paypal, poll_request())),
    payment_psp_mock:done().

reporting_order() ->
    C = fixture(paypal, <<"discovery-page">>),
    #{<<"transaction_details">> := [#{<<"transaction_info">> := Info} | _]} = maps:get(<<"response">>, C),
    R = (maps:get(<<"response">>, C))#{<<"transaction_details">> => [#{<<"transaction_info">> =>
        Info#{<<"paypal_reference_id_type">> => <<"ODR">>, <<"paypal_reference_id">> => <<"ORDERMOCK123">>}}],
        <<"total_pages">> => 1},
    C1 = C#{<<"response">> => R},
    Order = fixture(paypal, <<"get">>),
    payment_psp_mock:expect(paypal, with_oauth(paypal, C1) ++ with_oauth(paypal, Order)),
    {ok, [Data], undefined} = poll(paypal, poll_request()),
    ?assertEqual(<<"ORDERMOCK123">>, maps:get(psp_external_id, Data)),
    ?assertEqual(ok, payment_receive:validate(Data)),
    payment_psp_mock:done().


reporting_permission_error() ->
    ?LOG_NOTICE(#{in => zotonic_mod_payment,
        text => <<"Intentional test error follows: mocked PayPal reporting permission denial must be returned for retry.">>}),
    C = (fixture(paypal, <<"discovery-page">>))#{<<"status">> => 403,
        <<"response">> => #{<<"name">> => <<"NOT_AUTHORIZED">>,
            <<"message">> => <<"Authorization failed due to insufficient permissions.">>}},
    payment_psp_mock:expect(paypal, with_oauth(paypal, C)),
    ?assertEqual({error, 403}, poll(paypal, poll_request())),
    payment_psp_mock:done().

reporting_capture() ->
    C = fixture(paypal, <<"discovery-page">>),
    #{<<"transaction_details">> := [#{<<"transaction_info">> := Info} | _]} = maps:get(<<"response">>, C),
    C1 = C#{<<"response">> => #{<<"transaction_details">> => [#{<<"transaction_info">> =>
        Info#{<<"transaction_event_code">> => <<"T0006">>, <<"transaction_id">> => <<"CAPTUREMOCK123">>}}],
        <<"total_pages">> => 1}},
    Capture = fixture(paypal, <<"get-capture">>),
    Order = fixture(paypal, <<"get">>),
    payment_psp_mock:expect(paypal, with_oauth(paypal, C1) ++ with_oauth(paypal, Capture)
        ++ with_oauth(paypal, Order)),
    {ok, [Data], undefined} = poll(paypal, poll_request()),
    ?assertEqual(<<"ORDERMOCK123">>, maps:get(psp_external_id, Data)),
    payment_psp_mock:done().

reporting_pending_sync() ->
    C = fixture(paypal, <<"discovery-page">>),
    #{<<"transaction_details">> := [Record | _]} = maps:get(<<"response">>, C),
    #{<<"transaction_info">> := Info} = Record,
    C1 = C#{<<"path">> => <<"/v1/reporting/transactions?start_date=2026-10-01T11%3A59%3A59Z&end_date=2026-10-01T12%3A00%3A01Z&fields=transaction_info&balance_affecting_records_only=Y&transaction_id=9AB12345CD678901E">>,
        <<"response">> => #{<<"transaction_details">> => [Record]}},
    meck:new(mod_payment, [passthrough, no_link]),
    try
        payment_test_mock:expect(mod_payment, receive_payment, psp, fun(mod_payment_paypal, Data, _) ->
            ?assertEqual(paid, maps:get(status, Data)),
            ?assertEqual(<<"capture:9AB12345CD678901E">>, maps:get(psp_external_id, Data)),
            ?assertEqual(ok, payment_receive:validate(Data)),
            {ok, #{}}
        end),
        payment_psp_mock:expect(paypal, with_oauth(paypal, C1)),
        ?assertEqual(ok, psp_call(mod_payment_paypal, observe_payment_psp_status_sync, [
            #payment_psp_status_sync{payment_id = 1, psp_module = mod_payment_paypal,
                psp_external_id = <<"capture:9AB12345CD678901E">>,
                psp_data = #{<<"reporting_transaction">> => Info}}, #context{}])),
        payment_psp_mock:done()
    after meck:unload(mod_payment) end.


mollie_retention() ->
    payment_test_mock:expect(m_config, get_value, psp, fun
        (mod_payment, delete_after_days, _) -> <<"2">>;
        (mod_payment_mollie, sync_periodic, _) -> <<>>;
        (mod_payment_mollie, api_key, _) -> <<"test_mock">>
    end),
    payment_test_mock:expect(m_config, set_value, psp, fun(_, _, _, _) -> ok end),
    meck:new(m_payment, [passthrough, no_link]),
    try
        payment_test_mock:expect(m_payment, get_by_psp, psp, fun(mod_payment_mollie, _, _) -> {error, notfound} end),
        payment_test_mock:expect(m_payment, get, psp, fun(_, _) -> error(old_payment_imported) end),
        C = fixture(mollie, <<"list-payments">>),
        #{<<"_embedded">> := #{<<"payments">> := [Payment | _]}} = maps:get(<<"response">>, C),
        Old = Payment#{<<"createdAt">> => <<"2000-01-01T00:00:00+00:00">>},
        Response = #{<<"_embedded">> => #{<<"payments">> => [Old]}, <<"_links">> => #{<<"next">> => null}},
        payment_psp_mock:expect(mollie, [C#{<<"response">> => Response}]),
        ?assertEqual(ok, psp_call(m_payment_mollie_api, payment_sync_periodic, [#context{}])),
        payment_psp_mock:done()
    after meck:unload(m_payment) end.

%% PSP modules are optional: each CI job installs only its selected provider.
%% Resolve calls at runtime so standalone mod_payment analysis needs no PSPs.
psp_call(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

%% Documentation-shaped event fixtures deliberately refer to payments created
%% before the polling window. Partial Stripe refunds must not fully reverse them.
late_discovery(stripe) ->
    A = fixture(stripe, <<"discovery-late-event">>),
    B = fixture(stripe, <<"discovery-late-refund">>),
    S = fixture(stripe, <<"refund-session">>),
    % Stripe lists newest events first, including across page boundaries.
    payment_psp_mock:expect(stripe, [B, S, A]),
    {ok, [Refund], Next} = poll(stripe, (poll_request())#payment_psp_poll{cursor = {events, undefined}}),
    {ok, [Paid], undefined} = poll(stripe, (poll_request())#payment_psp_poll{cursor = Next}),
    ?assertEqual(paid, maps:get(status, Paid)),
    ?assertEqual({{2026,10,1},{12,0,0}}, maps:get(updated_at, Paid)),
    ?assertEqual(refunded, maps:get(status, Refund)),
    ?assertEqual(<<"cs_old">>, maps:get(psp_external_id, Refund)),
    ?assertEqual({{2026,10,1},{13,0,0}}, maps:get(updated_at, Refund)),
    ?assertEqual(ok, payment_receive:validate(Refund)),
    payment_psp_mock:done();
late_discovery(mollie) ->
    A = fixture(mollie, <<"discovery-late-refund">>),
    B = fixture(mollie, <<"refund-payment">>),
    C = fixture(mollie, <<"discovery-refund-next">>),
    payment_psp_mock:expect(mollie, [A, B, C]),
    {ok, [Refund], Next} = poll(mollie, (poll_request())#payment_psp_poll{cursor = {refunds, undefined}}),
    ?assertEqual(refunded, maps:get(status, Refund)),
    ?assertEqual(<<"tr_old">>, maps:get(psp_external_id, Refund)),
    ?assertEqual({{2026,10,1},{12,0,0}}, maps:get(updated_at, Refund)),
    ?assertEqual(ok, payment_receive:validate(Refund)),
    ?assertEqual({ok, [], undefined}, poll(mollie, (poll_request())#payment_psp_poll{cursor = Next})),
    payment_psp_mock:done();
late_discovery(paypal) ->
    C = fixture(paypal, <<"discovery-page">>),
    #{<<"transaction_details">> := [#{<<"transaction_info">> := Info} | _]} = maps:get(<<"response">>, C),
    Updated = Info#{<<"transaction_initiation_date">> => <<"2000-01-01T00:00:00Z">>,
                   <<"transaction_status">> => <<"V">>},
    Case = C#{<<"response">> => #{<<"transaction_details">> => [#{<<"transaction_info">> => Updated}],
                                  <<"total_pages">> => 1}},
    payment_psp_mock:expect(paypal, with_oauth(paypal, Case)),
    {ok, [Data], undefined} = poll(paypal, poll_request()),
    ?assertEqual({{2000,1,1},{0,0,0}}, maps:get(created_at, Data)),
    ?assertEqual({{2026,10,1},{12,5,0}}, maps:get(updated_at, Data)),
    ?assertEqual(refunded, maps:get(status, Data)),
    payment_psp_mock:done().
