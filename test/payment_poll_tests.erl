%% @copyright 2026 Marc Worrell
%% @doc Shared discovery policy and retry tests. Runs inside the test sandbox.
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

-module(payment_poll_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/payment.hrl").

poll_test_() ->
    {foreach, fun setup/0, fun(_) -> cleanup() end,
        [fun gates/0, fun pages_and_known/0, fun failed_page_retries/0,
         fun failed_import_retries/0, fun cursor_loop/0, fun catchup_window/0,
         fun disable_during_poll/0, fun retention/0, fun retention_disabled/0, fun missing_creation_date/0, fun updated_retention/0]}.

setup() ->
    meck:new([m_config, m_payment, mod_payment, z_notifier, z_sidejob], [passthrough, no_link]),
    mock(m_config, get_boolean, fun(_, K, _) ->
        case K of
            accept_unknown_payments -> get(accept) =/= false;
            disable_unknown_payment_polling -> get(disable) =:= true
        end
    end),
    mock(m_config, get_value, fun(mod_payment, delete_after_days, _) -> get(retention_days) end),
    mock(m_config, get_prop, fun(_, _, _, _) -> get(checkpoint) end),
    mock(m_config, set_prop, fun(_, _, _, T, _) -> put(checkpoint, T), ok end),
    mock(m_payment, get_by_psp, fun(_, Id, _) ->
        case Id of <<"known">> -> {ok, #{}}; _ -> {error, notfound} end
    end),
    mock(mod_payment, receive_payment, fun(_, Data, _) ->
        case get(import_failure) of
            true -> {error, db_failure};
            _ -> put(imported, [Data | case get(imported) of undefined -> []; Ds -> Ds end]), {ok, #{}}
        end
    end),
    mock(z_sidejob, start_site_unique, fun(payment_poll, {payment_poll, run, []}, _) ->
        put(started, true), {ok, self()}
    end),
    ok.

mock(M, F, Fun) -> payment_test_mock:expect(M, F, psp, Fun).
cleanup() -> meck:unload([m_config, m_payment, mod_payment, z_notifier, z_sidejob]).

page(Fun) -> mock(z_notifier, first, Fun).
poll() -> payment_poll:poll_provider(mod_payment_example, 2000000, #context{}).

%% EUnit runs each body in a fresh process; state is local to that test.
gates() ->
    put(accept, false),
    ?assertEqual(ok, payment_poll:start(#context{})),
    ?assertEqual({error, disabled}, poll()),
    ?assertEqual(undefined, get(started)),
    put(accept, true), put(disable, true),
    ?assertEqual(ok, payment_poll:start(#context{})),
    ?assertEqual(undefined, get(started)),
    put(disable, false),
    ?assertEqual(ok, mod_payment:observe_tick_4h(tick_4h, #context{})),
    ?assertEqual(true, get(started)).

pages_and_known() ->
    page(fun
        (#payment_psp_poll{cursor = undefined}, _) ->
            {ok, [#{psp_external_id => <<"known">>}, #{psp_external_id => <<"first">>}], next};
        (#payment_psp_poll{cursor = next}, _) ->
            {ok, [#{psp_external_id => <<"second">>}], undefined}
    end),
    ?assertEqual(ok, poll()),
    ?assertEqual(2000000, get(checkpoint)),
    ?assertEqual([#{psp_external_id => <<"second">>}, #{psp_external_id => <<"first">>}], get(imported)).

failed_page_retries() ->
    page(fun
        (#payment_psp_poll{cursor = undefined}, _) ->
            {ok, [#{psp_external_id => <<"before-failure">>}], second};
        (#payment_psp_poll{cursor = second}, _) -> {error, timeout}
    end),
    ?assertEqual({error, timeout}, poll()),
    ?assertEqual([#{psp_external_id => <<"before-failure">>}], get(imported)),
    Boundary = get(checkpoint),
    ?assertEqual(2000000 - 86400, Boundary),
    ?assertEqual({error, timeout}, payment_poll:poll_provider(mod_payment_example, 3000000, #context{})),
    ?assertEqual(Boundary, get(checkpoint)).

failed_import_retries() ->
    put(import_failure, true),
    page(fun(_, _) -> {ok, [#{psp_external_id => <<"new">>}], undefined} end),
    ?assertEqual({error, db_failure}, poll()),
    ?assertEqual(2000000 - 86400, get(checkpoint)).

cursor_loop() ->
    page(fun(_, _) -> {ok, [], same_cursor} end),
    ?assertEqual({error, repeated_cursor}, poll()),
    ?assertEqual(2000000 - 86400, get(checkpoint)).

catchup_window() ->
    put(checkpoint, 1000000),
    page(fun(#payment_psp_poll{since = Since, until = Until}, _) ->
        ?assertEqual(1000000 - 86400, z_datetime:datetime_to_timestamp(Since)),
        ?assertEqual(1000000 + 7 * 86400, z_datetime:datetime_to_timestamp(Until)),
        {ok, [], undefined}
    end),
    ?assertEqual(ok, poll()),
    ?assertEqual(1000000 + 7 * 86400, get(checkpoint)).

disable_during_poll() ->
    page(fun(_, _) -> put(disable, true), {ok, [#{psp_external_id => <<"new">>}], undefined} end),
    ?assertEqual({error, disabled}, poll()),
    ?assertEqual(undefined, get(imported)),
    ?assertEqual(2000000 - 86400, get(checkpoint)).


retention() ->
    put(retention_days, <<"2">>),
    put(checkpoint, 1000000),
    Cutoff = 2000000 - 2 * 86400,
    page(fun(#payment_psp_poll{since = Since, until = Until}, _) ->
        ?assertEqual(Cutoff, z_datetime:datetime_to_timestamp(Since)),
        ?assertEqual(2000000, z_datetime:datetime_to_timestamp(Until)),
        {ok, [dated(<<"old">>, Cutoff - 1), dated(<<"boundary">>, Cutoff),
              dated(<<"recent">>, Cutoff + 1)], undefined}
    end),
    ?assertEqual(ok, poll()),
    ?assertEqual([#{psp_external_id => <<"recent">>}, #{psp_external_id => <<"boundary">>}], get(imported)),
    ?assertEqual(2000000, get(checkpoint)).

retention_disabled() ->
    lists:foreach(fun(Value) ->
        put(retention_days, Value),
        put(checkpoint, 1000000),
        page(fun(#payment_psp_poll{since = Since}, _) ->
            ?assertEqual(1000000 - 86400, z_datetime:datetime_to_timestamp(Since)),
            {ok, [], undefined}
        end),
        ?assertEqual(ok, poll())
    end, [undefined, <<>>, <<"0">>, <<"1">>]).

missing_creation_date() ->
    put(retention_days, 30),
    page(fun(_, _) -> {ok, [#{psp_external_id => <<"undated">>}], undefined} end),
    ?assertEqual({error, payment_created_at}, poll()),
    ?assertEqual(undefined, get(imported)),
    ?assertEqual(2000000 - 86400, get(checkpoint)).

dated(Id, Time) ->
    #{psp_external_id => Id, created_at => z_datetime:timestamp_to_datetime(Time)}.

updated_retention() ->
    put(retention_days, 2),
    Old = z_datetime:timestamp_to_datetime(1000000),
    Recent = z_datetime:timestamp_to_datetime(1999999),
    Data = #{psp_external_id => <<"known">>, created_at => Old, updated_at => Recent},
    page(fun(_, _) -> {ok, [Data], undefined} end),
    ?assertEqual(ok, poll()),
    ?assertEqual([#{psp_external_id => <<"known">>}], get(imported)),
    put(imported, []),
    page(fun(_, _) -> {ok, [Data#{created_at => Recent, updated_at => Old}], undefined} end),
    ?assertEqual(ok, poll()),
    ?assertEqual([], get(imported)),
    page(fun(_, _) -> {ok, [Data#{updated_at => undefined}], undefined} end),
    ?assertEqual({error, payment_updated_at}, poll()).
