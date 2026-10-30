%% @copyright 2026 Marc Worrell
%% @doc Shared discovery of payments missed by webhooks. Providers only list and
%% normalize; scheduling, retries, checkpoints and imports belong here.
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

-module(payment_poll).
-export([start/1, run/1, is_enabled/1, is_within_retention/2]).
%% Exported for focused sandbox tests of retries and pagination, without a timer.
-export([poll_provider/3]).
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../../include/payment.hrl").

-define(POLL_DAY, 86400).

-spec is_enabled(z:context()) -> boolean().
is_enabled(Context) ->
    m_config:get_boolean(mod_payment, accept_unknown_payments, Context)
    andalso not m_config:get_boolean(mod_payment, disable_unknown_payment_polling, Context).

-spec start(z:context()) -> ok.
start(Context) ->
    case is_enabled(Context) of
        true ->
            _ = z_sidejob:start_site_unique(payment_poll, {?MODULE, run, []}, Context),
            ok;
        false -> ok
    end.

%% Called by the unique site job; a slow PSP must not hold up the cron notifier.
-spec run(z:context()) -> ok.
run(Context) ->
    case is_enabled(Context) of
        false -> ok;
        true ->
            Psps = lists:usort([P || P <- z_notifier:map(#payment_psp_poll{}, Context),
                                    is_atom(P), P =/= undefined]),
            Now = erlang:system_time(second),
            lists:foreach(fun(P) ->
                Result = try poll_provider(P, Now, Context)
                    catch Class:Reason:Stack -> {error, {Class, Reason, Stack}} end,
                case Result of
                    ok -> ok;
                    {error, Reason1} ->
                        ?LOG_WARNING(#{in => zotonic_mod_payment,
                            text => <<"Payment discovery failed; retrying on the next four-hour tick">>,
                            psp_module => P, result => error, reason => Reason1})
                end
            end, Psps)
    end.

%% Keep the first boundary even on failure. Subsequent windows overlap by a day
%% for delayed reporting (PayPal can lag by three hours). Catch up in seven-day
%% steps, keeping the reporting request below PayPal's 31-day maximum.
-spec poll_provider(module(), integer(), z:context()) -> ok | {error, term()}.
poll_provider(Psp, Now, Context) ->
    case is_enabled(Context) of
        false -> {error, disabled};
        true ->
            Previous = m_config:get_prop(mod_payment, payment_poll, Psp, Context),
            Cutoff = retention_cutoff(Now, Context),
            Last = retained_start(case Previous of
                undefined -> Now - ?POLL_DAY;
                T when is_integer(T) -> T
            end, Cutoff),
            case Previous of
                undefined -> ok = checkpoint(Psp, Last, Context);
                _ -> ok
            end,
            Until = min(Now, Last + 7 * ?POLL_DAY),
            Since = retained_start(case Previous of undefined -> Last; _ -> Last - ?POLL_DAY end, Cutoff),
            Request = #payment_psp_poll{psp_module = Psp,
                since = z_datetime:timestamp_to_datetime(Since),
                until = z_datetime:timestamp_to_datetime(Until)},
            case pages(Request, [], Cutoff, Context) of
                ok -> checkpoint(Psp, Until, Context);
                {error, _} = Error -> Error
            end
    end.

checkpoint(Psp, Time, Context) ->
    m_config:set_prop(mod_payment, payment_poll, Psp, Time, Context).

pages(_, Seen, _, _) when length(Seen) >= 1000 -> {error, page_limit};
pages(Request, Seen, Cutoff, Context) ->
    case is_enabled(Context) of
        false -> {error, disabled};
        true ->
            case z_notifier:first(Request, Context) of
                {ok, Payments, Next} when is_list(Payments) ->
                    case import(Payments, Request#payment_psp_poll.psp_module, Cutoff, Context) of
                        ok when Next =:= undefined -> ok;
                        ok ->
                            case lists:member(Next, [Request#payment_psp_poll.cursor | Seen]) of
                                true -> {error, repeated_cursor};
                                false -> pages(Request#payment_psp_poll{cursor = Next},
                                    [Request#payment_psp_poll.cursor | Seen], Cutoff, Context)
                            end;
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error;
                _ -> {error, poll_response}
            end
    end.

%% Creation-only lists discover identities. Provider update/event timestamps also
%% allow reconciliation of known payments through the usual status/date guards.
import([], _, _, _) -> ok;
import([Data | Rest], Psp, Cutoff, Context) ->
    case within_retention(Data, Cutoff) of
        true -> import_payment(Data, Rest, Psp, Cutoff, Context);
        false -> import(Rest, Psp, Cutoff, Context);
        {error, _} = Error -> Error
    end.

import_payment(#{psp_external_id := Id} = Data, Rest, Psp, Cutoff, Context) ->
    case is_enabled(Context) of
        false -> {error, disabled};
        true ->
            Result = case m_payment:get_by_psp(Psp, Id, Context) of
                {ok, _} when not is_map_key(updated_at, Data) -> ok;
                {ok, _} -> receive_payment(Psp, Data, Context);
                {error, notfound} -> receive_payment(Psp, Data, Context);
                {error, _} = Error -> Error
            end,
            case Result of
                ok -> import(Rest, Psp, Cutoff, Context);
                {error, _} = Error1 -> Error1
            end
    end.

receive_payment(Psp, Data, Context) ->
    case mod_payment:receive_payment(Psp, maps:without([created_at, updated_at], Data), Context) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.

%% Use the configured day count, including binary values from the config table.
%% Retention is disabled for unset values or counts below the cleanup minimum.
retention_cutoff(Now, Context) ->
    case m_config:get_value(mod_payment, delete_after_days, Context) of
        undefined -> undefined;
        <<>> -> undefined;
        Value ->
            case z_convert:to_integer(Value) of
                Days when is_integer(Days), Days > 1 -> Now - Days * ?POLL_DAY;
                Days when is_integer(Days) -> undefined;
                _ -> error(invalid_retention)
            end
    end.

retained_start(Time, undefined) -> Time;
retained_start(Time, Cutoff) -> max(Time, Cutoff).

%% Prefer the PSP modification/event date, falling back to creation for APIs
%% without change discovery. Never substitute the local observation time.
within_retention(_Data, undefined) -> true;
within_retention(#{updated_at := Date}, Cutoff) ->
    case within_retention(#{created_at => Date}, Cutoff) of
        {error, _} -> {error, payment_updated_at};
        Result -> Result
    end;
within_retention(#{created_at := {Date, {H, M, S}} = Created}, Cutoff)
    when is_tuple(Date), tuple_size(Date) =:= 3, is_integer(H), H >= 0, H < 24,
         is_integer(M), M >= 0, M < 60, is_integer(S), S >= 0, S < 60 ->
    case calendar:valid_date(Date) of
        true -> z_datetime:datetime_to_timestamp(Created) >= Cutoff;
        false -> {error, payment_created_at}
    end;
within_retention(_, _) -> {error, payment_created_at}.


%% @doc Shared age policy for legacy provider polling paths as well.
-spec is_within_retention(calendar:datetime(), z:context()) -> boolean() | {error, term()}.
is_within_retention(Created, Context) ->
    within_retention(#{created_at => Created}, retention_cutoff(erlang:system_time(second), Context)).
