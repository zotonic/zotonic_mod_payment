%% @doc Payment configuration UI. Reads and writes require use.mod_admin_config.
%% Only declared settings of active payment modules are editable. Credentials
%% are returned only to authorized admins and edited using password inputs.
-module(m_payment_config).
-behaviour(zotonic_model).
-export([m_get/3, event/2, save/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

m_get([<<"fields">>, Module | Rest], _Msg, Context) ->
    case authorized_schema(Module, Context) of
        {ok, M, Fields} -> {ok, {[field(M, F, Context) || F <- Fields], Rest}};
        {error, _} = Error -> Error
    end;
m_get(_, _, _) -> {error, unknown_path}.

event(#submit{message = {save, Args}}, Context) ->
    Module = proplists:get_value(module, Args),
    case save(Module, z_context:get_q_all_noz(Context), Context) of
        ok -> z_render:wire({reload, []}, z_render:growl(?__("Payment configuration saved.", Context), Context));
        {error, eacces} -> z_render:growl_error(?__("You are not allowed to configure payments.", Context), Context);
        {error, invalid_config} -> z_render:growl_error(?__("Check the configuration values. No settings were saved.", Context), Context);
        {error, _} -> z_render:growl_error(?__("Could not save the payment configuration.", Context), Context)
    end.

-spec save(atom() | binary(), list(), z:context()) -> ok | {error, term()}.
save(Module, Values, Context) ->
    case z_acl:is_admin_editable(Context) of
        true -> save_editable(Module, Values, Context);
        false -> {error, eacces}
    end.

save_editable(Module, Values, Context) ->
    case authorized_schema(Module, Context) of
        {ok, M, Fields} ->
            try
                Editable = [F || F <- Fields, not maps:get(is_readonly, F, false)],
                Allowed = [atom_to_binary(maps:get(key, F)) || F <- Editable],
                true = lists:all(fun({K, V}) -> lists:member(K, Allowed) andalso is_binary(V) end, Values),
                Updates = [update(M, F, Values) || F <- Editable],
                persist(M, Updates, Context)
            catch
                error:_ -> {error, invalid_config}
            end;
        {error, _} = Error ->
            Error
    end.

persist(Module, Updates, Context) ->
    lists:foreach(
        fun({K, V}) -> ok = m_config:set_value(Module, K, V, Context) end,
        Updates).

authorized_schema(Module, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        false ->
            {error, eacces};
        true ->
            Name = z_convert:to_binary(Module),
            case [M || M <- z_module_manager:active(Context), atom_to_binary(M) =:= Name,
                       is_payment_module(M)]
            of
                [M] -> {ok, M, z_module_manager:mod_config(M)};
                [] -> {error, eacces}
            end
    end.

is_payment_module(mod_payment) -> true;
is_payment_module(M) ->
    {M, Depends, _} = z_module_manager:dependencies(M),
    lists:member(mod_payment, Depends).

field(Module, #{key := Key} = F, Context) ->
    Default = maps:get(default, F, <<>>),
    Stored = m_config:get_value(Module, Key, Default, Context),
    Value = case maps:get(options, F, undefined) of
        undefined ->
            Stored;
        Options ->
            case lists:member(Stored, Options) of
                true -> Stored;
                false -> Default
            end
    end,
    Label = maps:get(label, F, binary:replace(atom_to_binary(Key), <<"_">>, <<" ">>, [global])),
    F#{
        label => Label,
        value => case maps:get(type, F) of
                    boolean -> z_convert:to_bool(Value);
                    _ -> Value
                 end
    }.

update(Module, #{key := Key, type := Type} = F, Values) ->
    Name = atom_to_binary(Key),
    Value = proplists:get_value(Name, Values, case Type of boolean -> <<"0">>; _ -> undefined end),
    true = case maps:get(options, F, undefined) of
        undefined -> true;
        Options -> lists:member(Value, Options)
    end,
    {Key, validate(Module, Key, Type, Value)}.

validate(_, _, integer, <<>>) -> <<>>;
validate(_, _, boolean, V) -> z_convert:to_bool(V);
validate(_, Key, integer, V) when is_binary(V) ->
    N = binary_to_integer(V),
    true = N >= 0,
    true = Key =/= delete_after_days orelse N >= 2,
    integer_to_binary(N);
validate(mod_payment, currency, binary, V) when is_binary(V) ->
    Currency = z_string:to_upper(z_string:trim(V)),
    true = mod_payment:is_valid_currency(Currency),
    Currency;
validate(mod_payment_sovergn, environment, binary, V) when V =:= <<"test">>; V =:= <<"live">> -> V;
validate(mod_payment_sovergn, environment, binary, _) -> error(invalid_environment);
validate(_, _, binary, V) when is_binary(V), byte_size(V) =< 4096 -> V.
