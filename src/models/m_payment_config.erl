%% @copyright 2026 Marc Worrell
%% @doc Payment configuration UI. Reads and writes require use.mod_admin_config.
%% Only declared settings of active payment modules are editable. Credentials
%% are returned only to authorized admins and edited using password inputs.
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

-module(m_payment_config).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "site_administrator", "model",
        "payments", "configuration", "configure"
    ]
}).
-moduledoc("
Expose editable configuration for active payment modules.

```django
{% for field in m.payment_config.fields.mod_payment %}
    {{ field.label|escape }}
{% endfor %}
```

The `fields[module]` path returns field maps derived from the module's `-mod_config`
declaration, including current values. Access requires `use.mod_admin_config`;
secret values are included for these authorized administrators. Only mod_payment
and active modules directly depending on it are accepted.

`save(Module, Values, Context)` accepts a list of binary form keys and values,
validates the complete submission and then writes settings individually. It also
requires `z_acl:is_admin_editable/1`. Returns `ok`, `{error, eacces}` or
`{error, invalid_config}`. Unknown and read-only fields are rejected. Unchecked
checkboxes submit an empty binary; booleans are normalized before saving. Currency
values are trimmed and uppercased. Empty credential fields clear their values.
The submit message `{save, [{module, Module}]}` uses the same save API.
").


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
        {error, invalid_config} -> z_render:growl_error(?__("Check the configuration values. No settings were saved.", Context), Context)
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
