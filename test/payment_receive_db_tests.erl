%% @copyright 2026 Marc Worrell
%% Run against an isolated PostgreSQL database with PAYMENT_TEST_PORT set.
%% The test owns only the payment_receive_test schema in that database.
%% @doc Database integration tests for receiving PSP payments.
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
    meck:new(z_db_pool, [passthrough, no_link]),
    payment_test_mock:expect(z_db_pool, get_connection, payment, fun(Ctx) -> {ok, z_context:get(worker, Ctx)} end),
    payment_test_mock:expect(z_db_pool, return_connection, payment, fun(_, _) -> ok end),
    payment_test_mock:expect(z_db_pool, get_database_options, payment, fun(_) -> Args end),
    meck:new(z_depcache, [passthrough, no_link]),
    payment_test_mock:expect(z_depcache, get, payment, fun(_, _) -> undefined end),
    payment_test_mock:expect(z_depcache, set, payment, fun(_, _, _, _, _) -> ok end),
    payment_test_mock:expect(z_depcache, flush, payment, fun(_, _) -> ok end),
    meck:new(z_stats, [passthrough, no_link]),
    payment_test_mock:expect(z_stats, record_event, payment, fun(_, _, _) -> ok end),
    payment_test_mock:expect(z_stats, record_duration, payment, fun(_, _, _, _) -> ok end),
    meck:new(z_notifier, [passthrough, no_link]),
    payment_test_mock:expect(z_notifier, first, payment, fun
        (#payment_psp_poll{}, _) -> {ok, [get(poll_payment)], undefined};
        (_, _) -> undefined
    end),
    payment_test_mock:expect(z_notifier, notify_queue, payment, fun(_) -> ok end),
    payment_test_mock:expect(z_notifier, notify_queue_flush, payment, fun(_) -> ok end),
    payment_test_mock:expect(z_notifier, notify, payment, fun(N, Ctx) -> z_context:get(owner, Ctx) ! N, ok end),
    meck:new(m_config, [passthrough, no_link]),
    payment_test_mock:expect(m_config, get_boolean, payment, fun
        (mod_payment, accept_unknown_payments, Ctx) -> z_context:get(accept, Ctx);
        (mod_payment, disable_unknown_payment_polling, Ctx) -> z_context:get(disable_polling, Ctx) =:= true
    end),
    payment_test_mock:expect(m_config, get_prop, payment, fun(_, _, _, _) -> get(poll_checkpoint) end),
    payment_test_mock:expect(m_config, set_prop, payment, fun(_, _, _, T, _) -> put(poll_checkpoint, T), ok end),
    payment_test_mock:expect(m_config, get_value, payment, fun
        (mod_payment, email_paid, _) -> undefined;
        (mod_payment, delete_after_days, Ctx) -> z_context:get(retention_days, Ctx)
    end),
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
    %% Test an existing installation missing the new column, then idempotence.
    z_db:q("alter table payment drop column psp_profile_id", C),
    z_db:flush(C),
    ok = m_payment:install(C),
    ok = m_payment:install(C),
    {ok, ProfileId} = z_db:insert(payment, #{<<"payment_nr">> => <<"profile-test">>,
        <<"amount">> => 1, <<"psp_module">> => mod_payment_mollie}, C),
    Handler = #payment_psp_handler{psp_module = mod_payment_mollie,
        psp_external_id = <<"tr_profile">>, psp_profile_id = <<"pfl_test">>,
        psp_data = #{}, redirect_uri = <<>>},
    ok = m_payment:update_psp_handler(ProfileId, Handler, C),
    ok = m_payment:update_psp_profile(ProfileId, undefined, C),
    {ok, ProfilePayment} = m_payment:get(ProfileId, C),
    ?assertEqual(<<"pfl_test">>, maps:get(<<"psp_profile_id">>, ProfilePayment)),
    ?assertEqual([#{<<"psp_module">> => mod_payment_mollie,
        <<"psp_profile_id">> => <<"pfl_test">>}], m_payment:profiles(C)),
    Headers = payment_export:headers(),
    Values = payment_export:values(ProfilePayment, C),
    ?assertEqual(length(Headers), length(Values)),
    ?assertEqual(<<"pfl_test">>, proplists:get_value(psp_profile_id, lists:zip(Headers, Values))),
    z_db:q("delete from payment where id = $1", [ProfileId], C),
    D = #{psp_external_id => <<"external-1">>, amount => 12.34, currency => <<"EUR">>,
          status => paid, status_date => {{2026, 10, 29}, {10, 0, 0}}, event_id => <<"evt-1">>,
          contact => #{<<"email">> => <<"payer@example.com">>}},
    ?assertEqual({error, notfound}, mod_payment:receive_payment(mod_payment_stripe, D,
        z_context:set(accept, false, C))),
    ?assertEqual(0, z_db:q1("select count(*) from payment", C)),
    {ok, P} = mod_payment:receive_payment(mod_payment_stripe, D, C),
    Id = maps:get(<<"id">>, P),
    ?assertEqual(undefined, maps:get(<<"psp_profile_id">>, P)),
    ?assertEqual([], m_payment:profiles(C)),
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
        <<"amount">> => 12.34}, C)),
    polling(D, C),
    modification_retention(C),
    succeeded_totals(C),
    discounts(C),
    stripe_refund_ordering(C),
    legacy_discounts(C),
    tied_events(C).

concurrency(D, C) ->
    Parent = self(),
    Pids = [spawn_monitor(fun() ->
        logger:set_process_metadata(#{site => payment_receive_test}),
        {ok, W} = z_db_pgsql:start_link(z_context:get(dbargs, C)),
        try Parent ! {received, mod_payment:receive_payment_transition(mod_payment_stripe, D,
            z_context:set(worker, W, C))}
        after gen_server:stop(W) end
    end) || _ <- lists:seq(1, 8)],
    Results = [receive {received, {ok, P, _, Change}} -> {maps:get(<<"id">>, P), Change}
           after 10000 -> error(concurrency_timeout) end || _ <- Pids],
    ?assertEqual(1, length(lists:usort([Id || {Id, _} <- Results]))),
    ?assertEqual(1, length([changed || {_, changed} <- Results])),
    [receive {'DOWN', Ref, process, Pid, normal} -> ok after 10000 -> error(worker_failed) end
     || {Pid, Ref} <- Pids],
    receive #payment_status{} -> ok after 1000 -> error(no_concurrent_notification) end,
    receive #payment_status{} -> error(duplicate_concurrent_notification) after 0 -> ok end.

%% Run the shared poller against the real receive transaction and unique index.
polling(D, C) ->
    put(poll_payment, D#{psp_external_id => <<"discovered">>, event_id => undefined}),
    Now = 1793275200,
    ?assertEqual({error, disabled}, payment_poll:poll_provider(mod_payment_example, Now,
        z_context:set(disable_polling, true, C))),
    ?assertEqual({error, notfound}, m_payment:get_by_psp(mod_payment_example, <<"discovered">>, C)),
    ?assertEqual(ok, payment_poll:poll_provider(mod_payment_example, Now, C)),
    {ok, #{<<"id">> := Id, <<"is_external">> := true, <<"status">> := paid}} =
        m_payment:get_by_psp(mod_payment_example, <<"discovered">>, C),
    receive #payment_status{payment_id = Id, previous_status = undefined} -> ok
    after 1000 -> error(no_poll_notification) end,
    ?assertEqual(Now, get(poll_checkpoint)),
    ?assertEqual(ok, payment_poll:poll_provider(mod_payment_example, Now + 14400, C)),
    ?assertEqual(1, z_db:q1("select count(*) from payment where psp_external_id = 'discovered'", C)),
    receive #payment_status{} -> error(duplicate_poll_notification) after 0 -> ok end,
    % Simulate cleanup, then ensure discovery does not recreate the old identity.
    z_db:q("delete from payment_log where payment_id = $1", [Id], C),
    z_db:q("delete from payment where id = $1", [Id], C),
    put(poll_payment, D#{psp_external_id => <<"discovered">>,
        created_at => z_datetime:timestamp_to_datetime(Now - 3 * 86400)}),
    ?assertEqual(ok, payment_poll:poll_provider(mod_payment_example, Now,
        z_context:set(retention_days, <<"2">>, C))),
    ?assertEqual({error, notfound}, m_payment:get_by_psp(mod_payment_example, <<"discovered">>, C)),
    receive #payment_status{} -> error(expired_poll_notification) after 0 -> ok end,
    % A recent PSP event can rediscover an old payment, then reconcile a refund.
    Recent = z_datetime:timestamp_to_datetime(Now),
    Event = (get(poll_payment))#{updated_at => Recent, status_date => Recent,
        event_id => <<"rediscovered">>},
    put(poll_payment, Event),
    RetentionContext = z_context:set(retention_days, <<"2">>, C),
    ?assertEqual(ok, payment_poll:poll_provider(mod_payment_example, Now, RetentionContext)),
    {ok, #{<<"id">> := NewId, <<"status">> := paid}} =
        m_payment:get_by_psp(mod_payment_example, <<"discovered">>, C),
    receive #payment_status{payment_id = NewId, previous_status = undefined} -> ok
    after 1000 -> error(no_rediscovery_notification) end,
    RefundDate = z_datetime:timestamp_to_datetime(Now + 1),
    put(poll_payment, Event#{status => refunded, updated_at => RefundDate,
        status_date => RefundDate, event_id => <<"discovered-refund">>}),
    ?assertEqual(ok, payment_poll:poll_provider(mod_payment_example, Now + 1, RetentionContext)),
    {ok, #{<<"status">> := refunded}} = m_payment:get(NewId, C),
    receive #payment_status{payment_id = NewId, previous_status = paid} -> ok
    after 1000 -> error(no_discovery_refund_notification) end,
    ?assertEqual(ok, payment_poll:poll_provider(mod_payment_example, Now + 1, RetentionContext)),
    receive #payment_status{} -> error(duplicate_discovery_refund) after 0 -> ok end.

%% A late refund restarts retention; repeated observations must not do so.
modification_retention(C) ->
    Old = {{2000, 1, 1}, {0, 0, 0}},
    {ok, Id} = z_db:insert(payment, #{
        <<"payment_nr">> => <<"retention-refund">>,
        <<"psp_module">> => mod_payment_stripe,
        <<"psp_external_id">> => <<"retention-psp">>,
        <<"amount">> => 12.34, <<"currency">> => <<"EUR">>,
        <<"status">> => paid, <<"status_date">> => Old,
        <<"created">> => Old, <<"modified">> => Old
    }, C),
    Data = #{psp_external_id => <<"retention-psp">>, status => refunded,
        status_date => {{2001, 1, 1}, {0, 0, 0}}, event_id => <<"late-refund">>},
    {ok, _} = mod_payment:receive_payment(mod_payment_stripe, Data, C),
    ?assert(modified(Id, C) > Old),
    ?assertEqual(Old, z_db:q1("select created from payment where id = $1", [Id], C)),
    ?assertEqual({ok, 0}, m_payment:delete_old(2, C)),
    % Make timestamp comparisons deterministic without sleeping.
    age_payment(Id, Old, C),
    {ok, _} = mod_payment:receive_payment(mod_payment_stripe, Data, C),
    ?assertEqual(Old, modified(Id, C)),
    {ok, _} = mod_payment:receive_payment(mod_payment_stripe,
        Data#{event_id => <<"refund-observed">>, status_date => {{2002, 1, 1}, {0, 0, 0}}}, C),
    ?assertEqual(Old, modified(Id, C)),
    ?assertEqual({ok, unchanged}, m_payment:set_payment_status(Id, paid, Old, C)),
    ?assertEqual(Old, modified(Id, C)),
    ?assertEqual({ok, 1}, m_payment:delete_old(2, C)),
    ?assertEqual({error, notfound}, m_payment:get(Id, C)),
    contact_and_handler_modification(Old, C).

contact_and_handler_modification(Old, C) ->
    {ok, Id} = z_db:insert(payment, #{<<"payment_nr">> => <<"retention-contact">>,
        <<"is_payment_link">> => true, <<"amount">> => 12.34,
        <<"currency">> => <<"EUR">>, <<"modified">> => Old}, C),
    Contact = #{<<"name_first">> => <<"Test">>, <<"email">> => <<"test@example.com">>,
        <<"address_street_1">> => <<"Street 1">>, <<"address_postcode">> => <<"1234AB">>,
        <<"address_city">> => <<"Amsterdam">>, <<"address_country">> => <<"nl">>},
    ?assertEqual(ok, m_payment:maybe_update_contact(Id, Contact, C)),
    ?assert(modified(Id, C) > Old),
    age_payment(Id, Old, C),
    ?assertEqual(ok, m_payment:maybe_update_contact(Id, Contact, C)),
    ?assertEqual(Old, modified(Id, C)),
    Handler = #payment_psp_handler{psp_module = mod_payment_stripe,
        psp_external_id = <<"contact-session">>, psp_data = #{}, redirect_uri = <<>>},
    ?assertEqual(ok, m_payment:update_psp_handler(Id, Handler, C)),
    ?assert(modified(Id, C) > Old),
    age_payment(Id, Old, C),
    ?assertEqual(ok, m_payment:update_psp_handler(Id, Handler, C)),
    ?assertEqual(Old, modified(Id, C)),
    UserId = z_db:q1("insert into rsc default values returning id", C),
    z_db:q("update payment set user_id = $2, is_recurring_start = true where id = $1", [Id, UserId], C),
    ?assertEqual(1, m_payment:cancel_recurring_payment(UserId, C)),
    ?assert(modified(Id, C) > Old),
    age_payment(Id, Old, C),
    ?assertEqual(0, m_payment:cancel_recurring_payment(UserId, C)),
    ?assertEqual(Old, modified(Id, C)).

age_payment(Id, Date, C) ->
    1 = z_db:q("update payment set modified = $2 where id = $1", [Id, Date], C).

modified(Id, C) ->
    z_db:q1("select modified from payment where id = $1", [Id], C).

succeeded_totals(C) ->
    z_db:q("delete from payment_log", C),
    z_db:q("delete from payment", C),
    lists:foreach(fun({Nr, Currency, Amount, Status, Reference, Profile, Date}) ->
        {ok, _} = z_db:insert(payment, #{<<"payment_nr">> => Nr,
            <<"currency">> => Currency, <<"amount">> => Amount, <<"status">> => Status,
            <<"reference">> => Reference, <<"psp_profile_id">> => Profile, <<"created">> => Date}, C)
    end, [
        {<<"sum-1">>, <<"EUR">>, 10.10, paid, <<"campaign-1">>, <<"pfl_one">>, {{2026,1,1},{0,0,0}}},
        {<<"sum-2">>, <<"EUR">>, 20.20, paid, <<"campaign-2">>, <<"pfl_one">>, {{2026,2,1},{0,0,0}}},
        {<<"sum-3">>, <<"USD">>, 5.0, paid, <<"other">>, <<"pfl_two">>, {{2025,1,1},{0,0,0}}},
        {<<"sum-4">>, <<"EUR">>, 100.0, refunded, <<"campaign-1">>, <<"pfl_one">>, {{2026,1,1},{0,0,0}}},
        {<<"sum-5">>, <<"EUR">>, 100.0, pending, <<"campaign-1">>, <<"pfl_one">>, {{2026,1,1},{0,0,0}}}
    ]),
    Normalize = fun(Rows) -> [{maps:get(<<"currency">>, R), z_convert:to_float(maps:get(<<"amount">>, R))} || R <- Rows] end,
    ?assertEqual([{<<"EUR">>, 30.3}, {<<"USD">>, 5.0}], Normalize(m_payment:succeeded_totals(#{}, C))),
    [Euro | _] = m_payment:succeeded_totals(#{}, C),
    ?assertEqual(<<"30.30">>, filter_format_price:format_price(maps:get(<<"amount">>, Euro), C)),
    Filters = #{<<"reference">> => <<"campaign-">>, <<"year">> => <<"2026">>,
        <<"profile">> => <<"pfl_one">>, <<"page">> => 2, <<"pagelen">> => 1},
    ?assertEqual([{<<"EUR">>, 30.3}], Normalize(m_payment:succeeded_totals(Filters, C))),
    ?assertEqual([{<<"EUR">>, 10.1}], Normalize(m_payment:succeeded_totals(Filters#{<<"payment">> => <<"sum-1">>}, C))),
    ?assertEqual([], m_payment:succeeded_totals(#{<<"profile">> => <<"missing">>}, C)).


%% The same transaction handles local payments, early imports and late imports.
discounts(C) ->
    lists:foreach(fun({Kind, Discount}) ->
        Id = iolist_to_binary(io_lib:format("discount-~p-~p", [Kind, Discount])),
        Date = {{2026, 11, 19}, {10, 0, 0}},
        Open = #{psp_external_id => Id, amount => 50.0, currency => <<"EUR">>,
            original_amount => 50.0, discount_amount => 0.0, status => new,
            status_date => Date, event_id => <<Id/binary, "-open">>},
        case Kind of
            local ->
                {ok, _} = z_db:insert(payment, #{<<"payment_nr">> => Id,
                    <<"psp_module">> => mod_payment_stripe, <<"psp_external_id">> => Id,
                    <<"amount">> => 50.0, <<"currency">> => <<"EUR">>,
                    <<"props">> => #{<<"business_note">> => <<"keep">>}}, C);
            early -> {ok, _} = mod_payment:receive_payment(mod_payment_stripe, Open, C);
            late -> ok
        end,
        Details = #{<<"discounts">> => [#{<<"coupon">> => <<"coupon_test">>}],
            <<"amount_subtotal">> => 5000, <<"amount_discount">> => trunc(Discount * 100)},
        Paid = Open#{amount => 50.0 - Discount, discount_amount => Discount,
            discount_details => Details, status => paid,
            status_date => {{2026, 11, 19}, {11, 0, 0}}, event_id => <<Id/binary, "-paid">>},
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe, Paid, C),
        ?assertEqual(50.0 - Discount, maps:get(<<"amount">>, P)),
        ?assertEqual(paid, maps:get(<<"status">>, P)),
        Props = maps:get(<<"props">>, P),
        ?assertEqual(50.0, maps:get(<<"original_amount">>, Props)),
        ?assertEqual(Discount, maps:get(<<"discount_amount">>, Props)),
        ?assertEqual(Details, maps:get(<<"discount_details">>, Props)),
        case Kind of
            local -> ?assertEqual(<<"keep">>, maps:get(<<"business_note">>, Props));
            _ -> ok
        end,
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe, Paid, C),
        %% Both old paid observations and late open events preserve settlement.
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe,
            Open#{status => paid, event_id => <<Id/binary, "-old-paid">>}, C),
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe,
            Open#{status_date => {{2026, 11, 19}, {12, 0, 0}},
                event_id => <<Id/binary, "-late-open">>}, C),
        ?assertEqual({error, payment_mismatch}, mod_payment:receive_payment(mod_payment_stripe,
            Paid#{original_amount => 60.0, amount => 60.0 - Discount,
                event_id => <<Id/binary, "-wrong-original">>}, C)),
        %% Refunds preserve the settled amount and its discount evidence.
        {ok, Refunded} = mod_payment:receive_payment(mod_payment_stripe,
            Paid#{status => refunded, status_date => {{2026, 11, 19}, {13, 0, 0}},
                event_id => <<Id/binary, "-refund">>}, C),
        ?assertEqual(50.0 - Discount, maps:get(<<"amount">>, Refunded)),
        ?assertEqual(Props, maps:get(<<"props">>, Refunded))
    end, [{Kind, Discount} || Kind <- [local, early, late], Discount <- [25.0, 50.0]]),
    ?assertEqual({error, notfound}, mod_payment:receive_payment(mod_payment_stripe,
        #{psp_external_id => <<"redirect-unknown">>, amount => 0.0, currency => <<"EUR">>,
          status => paid, status_date => {{2026, 11, 19}, {10, 0, 0}}, is_existing_only => true}, C)).

%% A refreshed Checkout Session still says paid after its charge is refunded.
stripe_refund_ordering(C) ->
    flush_status_notifications(),
    Session = legacy_session(<<"cs-refund-ordering">>, 2500),
    {ok, Paid} = mod_payment_stripe:normalize_payment(Session),
    T0 = maps:get(status_date, Paid),
    T1 = z_datetime:timestamp_to_datetime(1795082500),
    T2 = z_datetime:timestamp_to_datetime(1795082600),
    {ok, P} = mod_payment:receive_payment(mod_payment_stripe,
        Paid#{status => pending, status_date => T1, event_id => <<"pending-ordering">>}, C),
    Id = maps:get(<<"id">>, P),
    flush_status_notifications(),
    % Even when the session was created before the pending event, redirect settles it.
    ?assert(T0 < T1),
    ?assertMatch({ok, {_, paid}}, m_payment_stripe_api:sync_payment_session_status(Session, C)),
    receive #payment_status{payment_id = Id, status = paid} -> ok after 1000 -> error(no_paid) end,
    ?assertMatch({ok, {_, paid}}, m_payment_stripe_api:sync_payment_session_status(Session, C)),
    ?assertEqual(T1, z_db:q1("select status_date from payment where id = $1", [Id], C)),
    Refund = Paid#{status => refunded, status_date => T2, event_id => <<"delayed-refund">>},
    {ok, Refunded} = mod_payment:receive_payment(mod_payment_stripe, Refund, C),
    ?assertEqual(refunded, maps:get(<<"status">>, Refunded)),
    receive #payment_status{payment_id = Id, status = refunded} -> ok after 1000 -> error(no_refund) end,
    % Duplicate refund, old paid webhook, redirect and polling snapshot all preserve it.
    {ok, Refunded} = mod_payment:receive_payment(mod_payment_stripe, Refund, C),
    {ok, Refunded} = mod_payment:receive_payment(mod_payment_stripe,
        Paid#{status_date => T1, event_id => <<"late-paid-ordering">>}, C),
    ?assertMatch({ok, {_, refunded}}, m_payment_stripe_api:sync_payment_session_status(Session, C)),
    {ok, Refunded} = mod_payment:receive_payment(mod_payment_stripe,
        Paid#{is_status_snapshot => true}, C),
    receive #payment_status{} -> error(duplicate_refund_notification) after 0 -> ok end.

legacy_discounts(C) ->
    lists:foreach(fun({Kind, Discount}) ->
        Nr = iolist_to_binary(io_lib:format("legacy-~p-~p", [Kind, Discount])),
        Raw = legacy_session(Nr, Discount),
        {ok, D} = mod_payment_stripe:normalize_payment(Raw),
        {ok, Id} = z_db:insert(payment, #{<<"payment_nr">> => Nr,
            <<"psp_module">> => mod_payment_stripe, <<"psp_external_id">> => Nr,
            <<"status">> => paid, <<"status_date">> => maps:get(status_date, D),
            <<"currency">> => <<"EUR">>, <<"amount">> => maps:get(amount, D),
            <<"is_external">> => Kind =:= imported, <<"psp_data">> => ?DB_PROPS(Raw),
            <<"props">> => #{<<"business_note">> => <<"keep">>}}, C),
        Refund = D#{status => refunded, event_id => <<Nr/binary, "-refund">>,
            status_date => {{2026,11,20},{0,0,0}}},
        case Kind of
            local ->
                ?assertEqual({error, payment_mismatch}, mod_payment:receive_payment(mod_payment_stripe, Refund, C));
            imported ->
                ?assertEqual({error, payment_mismatch}, mod_payment:receive_payment(mod_payment_stripe,
                    Refund#{original_amount => 60.0, discount_amount => maps:get(discount_amount, D) + 10.0}, C)),
                {ok, P} = mod_payment:receive_payment(mod_payment_stripe, Refund, C),
                ?assertEqual(refunded, maps:get(<<"status">>, P)),
                ?assertEqual(maps:get(amount, D), maps:get(<<"amount">>, P)),
                Props = maps:get(<<"props">>, P),
                ?assertEqual(50.0, maps:get(<<"original_amount">>, Props)),
                ?assertEqual(maps:get(discount_details, D), maps:get(<<"discount_details">>, Props)),
                ?assertEqual(<<"keep">>, maps:get(<<"business_note">>, Props)),
                ?assertEqual(Raw, maps:get(<<"psp_data">>, P)),
                {ok, P} = mod_payment:receive_payment(mod_payment_stripe, Refund, C),
                {ok, P} = m_payment:get(Id, C)
        end
    end, [{Kind, Discount} || Kind <- [imported, local], Discount <- [2500, 5000]]).

legacy_session(Id, Discount) ->
    #{<<"id">> => Id, <<"object">> => <<"checkout.session">>, <<"mode">> => <<"payment">>,
      <<"status">> => <<"complete">>, <<"payment_status">> =>
        case Discount of 5000 -> <<"no_payment_required">>; _ -> <<"paid">> end,
      <<"amount_total">> => 5000 - Discount, <<"currency">> => <<"eur">>,
      <<"total_details">> => #{<<"amount_discount">> => Discount}, <<"created">> => 1795082400}.

flush_status_notifications() ->
    receive #payment_status{} -> flush_status_notifications() after 0 -> ok end.

%% The provider can timestamp completion and refund in the same second. The
%% outcome and settled amount must not depend on their delivery order.
tied_events(C) ->
    lists:foreach(fun(Order) ->
        Nr = atom_to_binary(Order),
        Raw = legacy_session(Nr, 2500),
        {ok, D} = mod_payment_stripe:normalize_payment(Raw),
        Paid = D#{event_id => <<Nr/binary, "-paid">>},
        Refund = D#{status => refunded, event_id => <<Nr/binary, "-refund">>},
        Events = case Order of refund_first -> [Refund, Paid]; paid_first -> [Paid, Refund] end,
        [First, Second] = Events,
        {ok, _} = mod_payment:receive_payment(mod_payment_stripe, First, C),
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe, Second, C),
        ?assertEqual(refunded, maps:get(<<"status">>, P)),
        flush_status_notifications(),
        % A later duplicate paid observation also cannot resurrect the charge or
        % change the settled discount amount, even without a stable event ID.
        LaterPaid = Paid#{event_id => undefined, amount => 50.0, discount_amount => 0.0,
            status_date => {{2026,11,21},{0,0,0}}},
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe, LaterPaid, C),
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe, First, C),
        {ok, P} = mod_payment:receive_payment(mod_payment_stripe, Second, C),
        receive #payment_status{} -> error(duplicate_tied_notification) after 0 -> ok end
    end, [refund_first, paid_first]).
