%% Run against an isolated PostgreSQL database with PAYMENT_TEST_PORT set.
%% The test owns only the payment_receive_test schema in that database.
-module(payment_receive_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/payment.hrl").

receive_db_test_() ->
    case os:getenv("PAYMENT_TEST_PORT") of
        false -> [];
        Port -> {setup, fun() -> setup(list_to_integer(Port)) end, fun cleanup/1,
                 fun(C) -> {timeout, 60, fun() -> exercise(C) end} end}
    end.

setup(Port) ->
    application:ensure_all_started(epgsql),
    application:ensure_all_started(crypto),
    Pool = spawn(fun() -> receive stop -> ok end end),
    register(payment_receive_test_pool, Pool),
    Args = [{dbhost, os:getenv("PAYMENT_TEST_HOST", "127.0.0.1")}, {dbport, Port}, {dbuser, os:getenv("PAYMENT_TEST_USER", "payment_test")},
            {dbpassword, os:getenv("PAYMENT_TEST_PASSWORD", "unused")}, {dbdatabase, os:getenv("PAYMENT_TEST_DATABASE", "postgres")}, {dbschema, "payment_receive_test"}],
    {ok, Worker} = z_db_pgsql:start_link(Args),
    C0 = #context{site = payment_receive_test, db = {payment_receive_test_pool, z_db_pgsql}},
    C = z_context:set([{worker, Worker}, {dbargs, Args}, {accept, true}], C0),
    meck:new(z_db_pool, [no_link]),
    meck:expect(z_db_pool, get_connection, fun(Ctx) -> {ok, z_context:get(worker, Ctx)} end),
    meck:expect(z_db_pool, return_connection, fun(_, _) -> ok end),
    meck:expect(z_db_pool, get_database_options, fun(_) -> Args end),
    meck:new(z_depcache, [no_link]),
    meck:expect(z_depcache, get, fun(_, _) -> undefined end),
    meck:expect(z_depcache, set, fun(_, _, _, _, _) -> ok end),
    meck:expect(z_depcache, flush, fun(_, _) -> ok end),
    meck:new(z_stats, [no_link]),
    meck:expect(z_stats, record_event, fun(_, _, _) -> ok end),
    meck:expect(z_stats, record_duration, fun(_, _, _, _) -> ok end),
    meck:new(z_notifier, [no_link]),
    meck:expect(z_notifier, first, fun(_, _) -> undefined end),
    meck:expect(z_notifier, notify_queue, fun(_) -> ok end),
    meck:expect(z_notifier, notify_queue_flush, fun(_) -> ok end),
    meck:expect(z_notifier, notify, fun(N, Ctx) -> z_context:get(owner, Ctx) ! N, ok end),
    meck:new(m_config, [no_link]),
    meck:expect(m_config, get_boolean, fun(mod_payment, accept_unknown_payments, Ctx) ->
        z_context:get(accept, Ctx)
    end),
    meck:expect(m_config, get_value, fun(mod_payment, email_paid, _) -> undefined end),
    logger:set_process_metadata(#{site => payment_receive_test}),
    z_db:q("create schema payment_receive_test", C),
    z_db:q("create table rsc (id serial primary key)", C),
    ok = m_payment:install(C),
    ok = m_payment_log:install(C),
    C.

cleanup(C) ->
    logger:set_process_metadata(#{site => payment_receive_test}),
    z_db:q("drop schema payment_receive_test cascade", C),
    gen_server:stop(z_context:get(worker, C)),
    whereis(payment_receive_test_pool) ! stop,
    meck:unload([z_db_pool, z_depcache, z_stats, z_notifier, m_config]).

exercise(C0) ->
    logger:set_process_metadata(#{site => payment_receive_test}),
    C = z_context:set(owner, self(), C0),
    D = #{psp_external_id => <<"external-1">>, amount => 12.34, currency => <<"EUR">>,
          status => paid, status_date => {{2026, 10, 29}, {10, 0, 0}}, event_id => <<"evt-1">>,
          contact => #{<<"email">> => <<"payer@example.com">>}},
    ?assertEqual({error, notfound}, mod_payment:receive_payment(mod_payment_stripe, D,
        z_context:set(accept, false, C))),
    ?assertEqual(0, z_db:q1("select count(*) from payment", C)),
    {ok, P} = mod_payment:receive_payment(mod_payment_stripe, D, C),
    Id = maps:get(<<"id">>, P),
    ?assertEqual(true, maps:get(<<"is_external">>, P)),
    ?assertEqual(undefined, maps:get(<<"user_id">>, P)),
    ?assertEqual(undefined, maps:get(<<"key">>, P)),
    ?assertEqual(<<"payer@example.com">>, maps:get(<<"email">>, P)),
    receive #payment_status{payment_id = Id, is_external = true, previous_status = undefined} -> ok
    after 1000 -> error(no_notification) end,
    {ok, P} = mod_payment:receive_payment(mod_payment_stripe, D, C),
    ?assertEqual(1, z_db:q1("select count(*) from payment_log", C)),
    receive #payment_status{} -> error(duplicate_notification) after 0 -> ok end,
    ?assertEqual({error, payment_mismatch}, mod_payment:receive_payment(mod_payment_stripe,
        D#{amount => 99, event_id => <<"bad-amount">>}, C)),
    ?assertEqual({error, payment_mismatch}, mod_payment:receive_payment(mod_payment_paypal,
        D#{payment_nr => maps:get(<<"payment_nr">>, P)}, C)),
    ?assertEqual({error, payment_data}, mod_payment:receive_payment(mod_payment_stripe,
        maps:remove(amount, D#{psp_external_id => <<"missing-amount">>}), C)),
    {ok, P} = mod_payment:receive_payment(mod_payment_stripe,
        D#{status => pending, event_id => <<"late-open">>, status_date => {{2026, 10, 29}, {11, 0, 0}}}, C),
    {ok, P} = mod_payment:receive_payment(mod_payment_stripe,
        D#{status => failed, event_id => <<"old-failed">>, status_date => {{2026, 10, 29}, {9, 0, 0}}}, C),
    % Known payments continue updating after importing is disabled.
    {ok, Refunded} = mod_payment:receive_payment(mod_payment_stripe,
        D#{status => refunded, event_id => <<"refund">>, status_date => {{2026, 10, 30}, {10, 0, 0}}},
        z_context:set(accept, false, C)),
    ?assertEqual(refunded, maps:get(<<"status">>, Refunded)),
    receive #payment_status{status = refunded, previous_status = paid} -> ok after 1000 -> error(no_refund) end,
    concurrency(D#{psp_external_id => <<"concurrent">>, event_id => <<"concurrent-event">>}, C),
    % Existing locally initiated payments retain their business key and origin.
    {ok, LocalId} = z_db:insert(payment, #{<<"payment_nr">> => <<"local-reference">>,
        <<"psp_module">> => mod_payment_stripe, <<"psp_external_id">> => <<"local-psp">>,
        <<"key">> => <<"order-123">>, <<"amount">> => 12.34, <<"currency">> => <<"EUR">>}, C),
    LocalData = D#{psp_external_id => <<"local-psp">>, payment_nr => <<"local-reference">>,
                  event_id => <<"local-event">>},
    {ok, Local} = mod_payment:receive_payment(mod_payment_stripe, LocalData,
        z_context:set(accept, false, C)),
    ?assertEqual(false, maps:get(<<"is_external">>, Local)),
    ?assertEqual(<<"order-123">>, maps:get(<<"key">>, Local)),
    receive #payment_status{payment_id = LocalId, is_external = false} -> ok
    after 1000 -> error(no_local_notification) end,
    % A same-status observation advances the timestamp without a notification.
    {ok, _} = mod_payment:receive_payment(mod_payment_stripe,
        LocalData#{event_id => <<"newer-paid">>, status_date => {{2026, 10, 29}, {12, 0, 0}}}, C),
    {ok, StillPaid} = mod_payment:receive_payment(mod_payment_stripe,
        LocalData#{event_id => <<"older-failed">>, status => failed,
                   status_date => {{2026, 10, 29}, {11, 0, 0}}}, C),
    ?assertEqual(paid, maps:get(<<"status">>, StillPaid)),
    receive #payment_status{} -> error(stale_notification) after 0 -> ok end,
    % A failure after insertion rolls the import back, including its status/log.
    z_db:q("alter table payment_log add constraint reject_test_logs
            check (event <> 'receive') not valid", C),
    ?LOG_NOTICE(#{
        in => ?MODULE,
        text => <<"Intentional test errors follow: reject_test_logs forces a log insert failure to verify transaction rollback and no notification.">>
    }),
    ?assertMatch({error, _}, mod_payment:receive_payment(mod_payment_stripe,
        D#{psp_external_id => <<"rollback">>, event_id => <<"rollback-event">>}, C)),
    z_db:q("alter table payment_log drop constraint reject_test_logs", C),
    ?assertEqual({error, notfound}, m_payment:get_by_psp(mod_payment_stripe, <<"rollback">>, C)),
    receive #payment_status{} -> error(rolled_back_notification) after 0 -> ok end,
    % Simulate upgrade from schema 9: preserve records and recreate the new index.
    Columns = z_db:column_names(payment, C),
    Index = z_db:q1("select pg_get_indexdef('payment_psp_identity_key'::regclass)", C),
    z_db:q("drop index payment_psp_identity_key", C),
    ok = m_payment:install(C),
    ok = m_payment:install(C),
    ?assertEqual(Columns, z_db:column_names(payment, C)),
    ?assertEqual(3, z_db:q1("select count(*) from payment", C)),
    ?assertEqual(Index, z_db:q1("select pg_get_indexdef('payment_psp_identity_key'::regclass)", C)),
    ?LOG_NOTICE(#{
        in => ?MODULE,
        text => <<"Intentional test error follows: payment_psp_identity_key rejects a duplicate PSP payment to verify database uniqueness.">>
    }),
    ?assertMatch({error, _}, z_db:insert(payment, #{<<"payment_nr">> => <<"duplicate">>,
        <<"psp_module">> => mod_payment_stripe, <<"psp_external_id">> => <<"external-1">>,
        <<"amount">> => 12.34}, C)).

concurrency(D, C) ->
    Parent = self(),
    Pids = [spawn_monitor(fun() ->
        logger:set_process_metadata(#{site => payment_receive_test}),
        {ok, W} = z_db_pgsql:start_link(z_context:get(dbargs, C)),
        try Parent ! {received, mod_payment:receive_payment(mod_payment_stripe, D,
            z_context:set(worker, W, C))}
        after gen_server:stop(W) end
    end) || _ <- lists:seq(1, 8)],
    Ids = [receive {received, {ok, P}} -> maps:get(<<"id">>, P)
           after 10000 -> error(concurrency_timeout) end || _ <- Pids],
    ?assertEqual(1, length(lists:usort(Ids))),
    [receive {'DOWN', Ref, process, Pid, normal} -> ok after 10000 -> error(worker_failed) end
     || {Pid, Ref} <- Pids],
    receive #payment_status{} -> ok after 1000 -> error(no_concurrent_notification) end,
    receive #payment_status{} -> error(duplicate_concurrent_notification) after 0 -> ok end.
