-module(payment_config_tests).
-include_lib("eunit/include/eunit.hrl").

config_test_() ->
    {foreach, fun setup/0, fun(_) -> meck:unload([z_acl, z_module_manager, m_config, z_db]) end,
        [fun denied/0, fun secret_read/0, fun save_and_clear/0, fun invalid/0, fun select_options/0]}.

setup() ->
    meck:new([z_acl, z_module_manager, m_config, z_db], [passthrough, no_link]),
    mock(z_acl, is_allowed, fun(use, mod_admin_config, _) -> get(denied) =/= true end),
    mock(z_acl, is_admin_editable, fun(_) -> get(denied) =/= true andalso get(readonly) =/= true end),
    mock(z_module_manager, active, fun(_) -> [mod_payment] end),
    meck:expect(z_module_manager, mod_config, fun(mod_payment) -> [
        #{key => secret, type => binary, is_secret => true},
        #{key => enabled, type => boolean},
        #{key => amount, type => integer},
        #{key => cursor, type => binary, is_readonly => true}
    ]; (M) -> meck:passthrough([M]) end),
    mock(m_config, get_value, fun(_, secret, _, _) -> <<"configured-secret">>;
        (_, _, Default, _) -> Default end),
    mock(m_config, set_value, fun(_, K, V, _) -> put(K, V), ok end),
    mock(z_db, transaction, fun(F, C) -> F(C) end).

mock(M, F, Fun) -> payment_test_mock:expect(M, F, psp, Fun).
values() -> [{<<"secret">>, <<>>}, {<<"amount">>, <<"10">>}].

denied() ->
    put(denied, true),
    ?assertEqual({error, eacces}, m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, undefined)),
    ?assertEqual({error, eacces}, m_payment_config:save(mod_payment, values(), undefined)),
    ?assertEqual(undefined, get(amount)),
    put(denied, false),
    put(readonly, true),
    ?assertEqual({error, eacces}, m_payment_config:save(mod_payment, values(), undefined)),
    put(readonly, false),
    ?assertEqual({error, eacces}, m_payment_config:save(<<"mod_other">>, values(), undefined)).

secret_read() ->
    {ok, {[F | _], []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, undefined),
    ?assertEqual(<<"configured-secret">>, maps:get(value, F)).

save_and_clear() ->
    ?assertEqual(ok, m_payment_config:save(mod_payment, values(), undefined)),
    ?assertEqual(<<>>, get(secret)),
    ?assertEqual(<<"0">>, get(enabled)),
    ?assertEqual(<<"10">>, get(amount)),
    ?assertEqual(ok, m_payment_config:save(mod_payment,
        [{<<"enabled">>, <<"1">>} | values()], undefined)),
    ?assertEqual(<<>>, get(secret)),
    ?assertEqual(<<"1">>, get(enabled)),
    ?assertEqual(ok, m_payment_config:save(mod_payment,
        [{<<"secret">>, <<"replacement">>}, {<<"amount">>, <<>>}], undefined)),
    ?assertEqual(<<"replacement">>, get(secret)).

invalid() ->
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment,
        [{<<"cursor">>, <<"tampered">>} | values()], undefined)),
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment,
        [{<<"secret">>, <<"changed">>}, {<<"amount">>, <<"-1">>}], undefined)),
    ?assertEqual(undefined, get(secret)),
    ?assertEqual(undefined, get(amount)).

%% Exercise the actual config transaction, nested m_config writes and template
%% escaping in the same live sandbox used by CI.
save_sandbox_test() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, {Fields, []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, C),
    Original = [{maps:get(key, F), m_config:get_value(mod_payment, maps:get(key, F), C)} || F <- Fields],
    Values = [{atom_to_binary(maps:get(key, F)), case maps:get(type, F) of
        boolean -> case maps:get(value, F) of true -> <<"1">>; false -> <<"0">> end;
        _ -> z_convert:to_binary(maps:get(value, F))
    end} || F <- Fields],
    try
        ?assertEqual(ok, m_payment_config:save(mod_payment,
            lists:keystore(<<"description">>, 1, Values, {<<"description">>, <<"Admin <test>">>}), C)),
        ?assertEqual(<<"Admin <test>">>, m_config:get_value(mod_payment, description, C)),
        {Output, _} = z_render:output(z_template:render_block(content, "admin_payment_config.tpl", [], C), C),
        ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Output), <<"Admin &lt;test&gt;">>)),
        Anonymous = z_acl:anondo(C),
        ?assertEqual({error, eacces}, m_payment_config:save(mod_payment, Values, Anonymous)),
        {DeniedOutput, _} = z_render:output(z_template:render_block(content, "admin_payment_config.tpl", [], Anonymous), Anonymous),
        ?assertEqual(nomatch, binary:match(iolist_to_binary(DeniedOutput), <<"<form">>))
    after
        lists:foreach(fun
            ({K, undefined}) -> m_config:delete(mod_payment, K, C);
            ({K, V}) -> m_config:set_value(mod_payment, K, V, C)
        end, Original)
    end.


select_options() ->
    meck:expect(z_module_manager, mod_config, fun(mod_payment) -> [
        #{key => interval, type => binary, default => <<"yearly">>,
          options => [<<"monthly">>, <<"yearly">>]}
    ]; (M) -> meck:passthrough([M]) end),
    {ok, {[Field], []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, undefined),
    ?assertEqual(<<"yearly">>, maps:get(value, Field)),
    ?assertEqual(ok, m_payment_config:save(mod_payment, [{<<"interval">>, <<"monthly">>}], undefined)),
    ?assertEqual(<<"monthly">>, get(interval)),
    ?assertEqual(ok, m_payment_config:save(mod_payment, [{<<"interval">>, <<"yearly">>}], undefined)),
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment, [{<<"interval">>, <<"weekly">>}], undefined)),
    ?assertEqual(<<"yearly">>, get(interval)).
