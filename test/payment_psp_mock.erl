%% @copyright 2026 Marc Worrell
%% @doc Offline HTTP boundary for documented PSP contract fixtures.
%% Test-process requests never pass through; sandbox workers retain real IO.
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

-module(payment_psp_mock).
-export([start/0, stop/0, expect/2, done/0, fixtures/1, request/5]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    meck:new([httpc, z_url_fetch, m_config, z_depcache], [passthrough, no_link, unstick]),
    payment_test_mock:expect(m_config, get_value, psp, fun config/3),
    payment_test_mock:expect(m_config, get_boolean, psp, fun
        (_, is_live, _) -> false;
        (mod_payment_paypal, enable_reporting_polling, _) -> true
    end),
    payment_test_mock:expect(z_depcache, get, psp, fun(_, _) -> undefined end),
    payment_test_mock:expect(z_depcache, set, psp, fun(_, _, _, _) -> ok end),
    payment_test_mock:expect(z_depcache, flush, psp, fun(_, _) -> ok end),
    meck:expect(httpc, request, fun(M, R, H, O) ->
        case get(psp_requests) of
            undefined -> meck:passthrough([M, R, H, O]);
            _ -> httpc_request(M, R, H, O)
        end
    end),
    meck:expect(z_url_fetch, fetch, fun(M, U, B, O) ->
        case get(psp_requests) of
            undefined -> meck:passthrough([M, U, B, O]);
            _ -> fetch(M, U, B, O)
        end
    end),
    meck:expect(z_url_fetch, fetch, fun(U, O) ->
        case get(psp_requests) of
            undefined -> meck:passthrough([U, O]);
            _ -> fetch(get, U, <<>>, O)
        end
    end),
    ok.

stop() ->
    erase(psp_requests),
    meck:unload([httpc, z_url_fetch, m_config, z_depcache]).

config(mod_payment_mollie, api_key, _) -> <<"test_mock">>;
config(mod_payment_stripe, secret_key, _) -> <<"sk_test_mock">>;
config(mod_payment_buckaroo, website_key, _) -> <<"website_mock">>;
config(mod_payment_buckaroo, secret_key, _) -> <<"secret_mock">>;
config(mod_payment_buckaroo, is_live, _) -> false;
config(mod_payment_paypal, webhook_id, _) -> <<"WHMOCK">>;
config(mod_payment_paypal, client_id, _) -> <<"client_mock">>;
config(mod_payment_paypal, secret, _) -> <<"secret_mock">>;
config(mod_payment_sovergn, oauth_client_id, _) -> <<"client_mock">>;
config(mod_payment_sovergn, oauth_client_secret, _) -> <<"secret_mock">>.

fixtures(Provider) ->
    Root = os:getenv("PAYMENT_CONTRACT_DIR", "apps_user/zotonic_mod_payment/test/psp_contracts"),
    {ok, B} = file:read_file(filename:join(Root, atom_to_list(Provider) ++ ".json")),
    jsx:decode(B, [return_maps]).

expect(Provider, Cases) ->
    put(psp_requests, [{Provider, C} || C <- Cases]),
    ok.

done() -> ?assertEqual([], get(psp_requests)).

httpc_request(Method, Request, _, _) ->
    {Url, Headers, CT, Body} = case Request of
        {U, H} -> {U, H, <<>>, <<>>};
        {U, H, T, B} -> {U, H, T, B}
    end,
    Auth = proplists:get_value("Authorization", Headers),
    {Code, Response} = request(Method, Url, Body, Auth, CT),
    {ok, {{"HTTP/1.1", Code, "Mock"}, [{"content-type", "application/json"}], Response}}.

fetch(Method, Url, Body, Options) ->
    check_options(Url, Options),
    {Code, Response} = request(Method, Url, Body,
        proplists:get_value(authorization, Options),
        proplists:get_value(content_type, Options, <<>>)),
    Result = {Url, [{"content-type", "application/json"}], byte_size(Response), Response},
    case Code of
        C when C >= 200, C < 300 -> {ok, Result};
        _ -> {error, erlang:insert_element(1, Result, Code)}
    end.

request(Method, Url0, Body0, Auth0, CT0) ->
    Url = iolist_to_binary(Url0),
    Body = case Body0 of undefined -> <<>>; _ -> iolist_to_binary(Body0) end,
    Auth = iolist_to_binary(Auth0),
    CT = iolist_to_binary(CT0),
    case get(psp_requests) of
        [{Provider, C} | Rest] ->
            ?assertEqual(maps:get(<<"method">>, C), atom_to_binary(Method)),
            ?assertEqual(url(Provider, maps:get(<<"path">>, C)), Url),
            check_auth(Provider, Method, Url, Body, Auth, C),
            check_body(Provider, Method, Body, CT, maps:get(<<"request">>, C)),
            put(psp_requests, Rest),
            {maps:get(<<"status">>, C), jsx:encode(maps:get(<<"response">>, C))};
        Other -> error({unexpected_psp_request, Method, Url, Other})
    end.

url(_, <<"https://", _/binary>> = U) -> U;
url(mollie, P) -> <<"https://api.mollie.nl/v2/", P/binary>>;
url(stripe, P) -> <<"https://api.stripe.com", P/binary>>;
url(paypal, P) -> <<"https://api-m.sandbox.paypal.com", P/binary>>;
url(buckaroo, P) -> <<"https://testcheckout.buckaroo.nl/", P/binary>>;
url(sovergn, P) -> <<"https://beta.sovergnllc.com", P/binary>>.

check_auth(mollie, _, _, _, A, _) -> ?assertEqual(<<"Bearer test_mock">>, A);
check_auth(stripe, _, _, _, A, _) -> ?assertEqual(<<"Bearer sk_test_mock">>, A);
check_auth(buckaroo, M, U, B, A, _) ->
    %% Buckaroo JSON authenticator specification. Independent of adapter helpers.
    <<"hmac ", Parts/binary>> = A,
    [<<"website_mock">>, Sig, Nonce, Timestamp] = binary:split(Parts, <<":">>, [global]),
    ?assert(byte_size(Nonce) > 0),
    ?assert(abs(binary_to_integer(Timestamp) - erlang:system_time(second)) < 60),
    <<"https://", HostPath/binary>> = U,
    EncodedUrl = string:lowercase(uri_string:quote(HostPath)),
    Digest = case M of get -> <<>>; _ -> base64:encode(crypto:hash(md5, B)) end,
    Data = [<<"website_mock">>, string:uppercase(atom_to_binary(M)), EncodedUrl,
        Timestamp, Nonce, Digest],
    ?assertEqual(base64:encode(crypto:mac(hmac, sha256, <<"secret_mock">>, Data)), Sig);
check_auth(_, _, _, _, A, #{ <<"name">> := <<"oauth">> }) ->
    ?assertEqual(<<"Basic ", (base64:encode(<<"client_mock:secret_mock">>))/binary>>, A);
check_auth(_, _, _, _, A, _) -> ?assertEqual(<<"Bearer token_mock">>, A).

check_body(_, get, B, _, Expected) ->
    ?assertEqual(#{}, Expected),
    ?assertEqual(<<>>, B);
check_body(P, _, B, CT, Expected) when P =:= mollie; P =:= stripe ->
    ?assertEqual(<<"application/x-www-form-urlencoded">>, CT),
    ?assertEqual(Expected, maps:from_list(cow_qs:parse_qs(B)));
check_body(_, _, B, <<"application/x-www-form-urlencoded">>, Expected) ->
    ?assertEqual(Expected, maps:from_list(cow_qs:parse_qs(B)));
check_body(_, _, B, CT, Expected) ->
    ?assertEqual(<<"application/json">>, CT),
    ?assertEqual(Expected, jsx:decode(B, [return_maps])).

check_options(<<"https://beta.sovergnllc.com", _/binary>> = Url, Options) ->
    ?assertEqual(false, proplists:get_value(autoredirect, Options)),
    case Url of
        <<"https://beta.sovergnllc.com/v2/checkout_sessions">> ->
            ?assertEqual(<<"contract-test-key">>, proplists:get_value(<<"Idempotency-Key">>,
                proplists:get_value(headers, Options, [])));
        _ -> ok
    end;
check_options(_, _) -> ok.
