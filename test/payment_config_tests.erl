%% @copyright 2026 Marc Worrell
%% @doc Tests for payment configuration access, validation and persistence.
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

-module(payment_config_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

config_test_() ->
    {foreach, fun setup/0, fun(_) -> meck:unload([z_acl, z_module_manager, m_config]) end,
        [fun denied/0, fun secret_read/0, fun save_and_clear/0, fun boolean_values/0, fun invalid/0, fun select_options/0]}.

setup() ->
    meck:new([z_acl, z_module_manager, m_config], [passthrough, no_link]),
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
    mock(m_config, set_value, fun(_, K, V, _) -> put(K, V), ok end).

mock(M, F, Fun) -> payment_test_mock:expect(M, F, psp, Fun).
values() -> [{<<"secret">>, <<>>}, {<<"amount">>, <<"10">>}].

denied() ->
    put(denied, true),
    ?assertEqual({error, eacces}, m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, #context{})),
    ?assertEqual({error, eacces}, m_payment_config:save(mod_payment, values(), #context{})),
    ?assertEqual(undefined, get(amount)),
    put(denied, false),
    put(readonly, true),
    ?assertEqual({error, eacces}, m_payment_config:save(mod_payment, values(), #context{})),
    put(readonly, false),
    ?assertEqual({error, eacces}, m_payment_config:save(<<"mod_other">>, values(), #context{})).

secret_read() ->
    {ok, {[F | _], []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, #context{}),
    ?assertEqual(<<"configured-secret">>, maps:get(value, F)).

save_and_clear() ->
    ?assertEqual(ok, m_payment_config:save(mod_payment, values(), #context{})),
    ?assertEqual(<<>>, get(secret)),
    ?assertEqual(false, get(enabled)),
    ?assertEqual(<<"10">>, get(amount)),
    ?assertEqual(ok, m_payment_config:save(mod_payment, [{<<"enabled">>, <<"1">>} | values()], #context{})),
    ?assertEqual(<<>>, get(secret)),
    ?assertEqual(true, get(enabled)),
    ?assertEqual(ok, m_payment_config:save(mod_payment,
        [{<<"secret">>, <<"replacement">>}, {<<"amount">>, <<>>}], #context{})),
    ?assertEqual(<<"replacement">>, get(secret)),
    ?assertEqual(false, get(enabled)).

%% Browser form values are binaries; normalized config writes are booleans.
boolean_values() ->
    lists:foreach(fun({Submitted, Expected}) ->
        ?assertEqual(ok, m_payment_config:save(mod_payment,
            [{<<"enabled">>, Submitted} | values()], #context{})),
        ?assertEqual(Expected, get(enabled))
    end, [{<<>>, false}, {<<"1">>, true}, {<<"0">>, false}, {<<"true">>, true}, {<<"false">>, false}]),
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment,
        [{<<"enabled">>, true} | values()], #context{})).

invalid() ->
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment,
        [{<<"cursor">>, <<"tampered">>} | values()], #context{})),
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment,
        [{<<"secret">>, <<"changed">>}, {<<"amount">>, <<"-1">>}], #context{})),
    ?assertEqual(undefined, get(secret)),
    ?assertEqual(undefined, get(amount)).

%% Exercise actual config writes and template escaping in the live CI sandbox.
save_sandbox_test() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    {ok, {Fields, []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, C),
    Original = [{maps:get(key, F), m_config:get_value(mod_payment, maps:get(key, F), C)} || F <- Fields],
    Values = [{atom_to_binary(maps:get(key, F)), case maps:get(type, F) of
        boolean -> case maps:get(value, F) of true -> <<"1">>; false -> <<>> end;
        _ -> z_convert:to_binary(maps:get(value, F))
    end} || F <- Fields],
    try
        % zotonic-wired submits an empty value for unchecked checkboxes without
        % value-unchecked. Verify both states through storage and the model.
        EnabledValues = lists:keystore(<<"accept_unknown_payments">>, 1, Values,
            {<<"accept_unknown_payments">>, <<"1">>}),
        ?assertEqual(ok, m_payment_config:save(mod_payment, EnabledValues, C)),
        ?assertEqual(true, m_config:get_boolean(mod_payment, accept_unknown_payments, C)),
        ?assertEqual(true, config_boolean_field(accept_unknown_payments, C)),
        ?assertEqual(ok, m_payment_config:save(mod_payment,
            lists:keystore(<<"accept_unknown_payments">>, 1, EnabledValues,
                {<<"accept_unknown_payments">>, <<>>}), C)),
        ?assertEqual(<<"false">>, z_db:q1(
            "select value from config where module = $1 and key = $2",
            [mod_payment, accept_unknown_payments], C)),
        ?assertEqual(false, m_config:get_boolean(mod_payment, accept_unknown_payments, C)),
        ?assertEqual(false, config_boolean_field(accept_unknown_payments, C)),
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
    {ok, {[Field], []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, #context{}),
    ?assertEqual(<<"yearly">>, maps:get(value, Field)),
    ?assertEqual(ok, m_payment_config:save(mod_payment, [{<<"interval">>, <<"monthly">>}], #context{})),
    ?assertEqual(<<"monthly">>, get(interval)),
    ?assertEqual(ok, m_payment_config:save(mod_payment, [{<<"interval">>, <<"yearly">>}], #context{})),
    ?assertEqual({error, invalid_config}, m_payment_config:save(mod_payment, [{<<"interval">>, <<"weekly">>}], #context{})),
    ?assertEqual(<<"yearly">>, get(interval)).


config_boolean_field(Key, Context) ->
    {ok, {Fields, []}} = m_payment_config:m_get([<<"fields">>, <<"mod_payment">>], #{}, Context),
    [Value] = [V || #{key := K, value := V} <- Fields, K =:= Key],
    Value.
