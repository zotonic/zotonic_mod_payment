%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2026 Driebit BV
%% @doc Payment module. Interfacing to PSP modules.
%% @end

%% Copyright 2018-2026 Driebit BV
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

-module(mod_payment).

-mod_title("Payments").
-mod_description("Payment services using Payment Service Provider modules").
-mod_author("Driebit").
-mod_schema(8).

-author("Driebit <tech@driebit.nl>").

-export([
    event/2,
    observe_search_query/2,
    observe_payment_request/2,

    payment_request_from_query/4,

    observe_tick_24h/2,

    observe_export_resource_visible/2,
    observe_export_resource_filename/2,
    observe_export_resource_header/2,
    observe_export_resource_encode/2,
    observe_export_resource_data/2,

    observe_admin_menu/3,
    set_payment_status/3,
    set_payment_status/4,
    manage_schema/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").
-include("../include/payment.hrl").

-define(MAX_REFERENCE_LENGTH, 100).
-define(MAX_DESCRIPTION_LENGTH, 200).

%% @doc Submit a form post here to start payments.
event(#submit{message={payment, Args} }, Context) ->
    {key, Key} = proplists:lookup(key, Args),
    UserId = case proplists:get_value(user_id, Args) of
        undefined -> z_acl:user(Context);
        UId when is_integer(UId) -> UId
    end,
    case is_allowed(UserId, Context) of
        true ->
            PaymentRequest = payment_request_from_query(Key, UserId, Args, Context),
            case z_notifier:first(PaymentRequest, Context) of
                #payment_request_redirect{ redirect_uri = RedirectUri } ->
                    z_render:wire({redirect, [ {location, RedirectUri} ]}, Context);
                {error, _Reason} ->
                    z_render:wire(
                        {alert, [
                            {title, ?__("Sorry", Context)},
                            {text, ?__("Something went wrong whilst handling the payment request, please try again later.", Context)}
                        ]},
                        Context);
                undefined ->
                    z_render:wire(
                        {alert, [
                            {title, ?__("Sorry", Context)},
                            {text, ?__("At the moment we cannot handle payments, please try again later.", Context)}
                        ]},
                        Context)
            end;
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to do this.", Context), Context)
    end;
event(#submit{ message={cancel_recurring, Args} }, Context) ->
    UserId = proplists:get_value(user_id, Args, z_acl:user(Context)),
    case is_allowed(UserId, Context) of
        true ->
            case z_notifier:first(#cancel_recurring_psp_request{ user_id = UserId }, Context) of
                ok -> m_payment:cancel_recurring_payment(UserId, Context);
                _ -> noop
            end,
            z_render:wire({redirect, [ {location, m_rsc:page_url(UserId, Context)} ]}, Context);
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to do this.", Context), Context)
    end;
event(#postback{ message={cancel_recurring, Args} }, Context) ->
    UserId = proplists:get_value(user_id, Args, z_acl:user(Context)),
    case is_allowed(UserId, Context) of
        true ->
            case z_notifier:first(#cancel_recurring_psp_request{ user_id = UserId }, Context) of
                ok -> m_payment:cancel_recurring_payment(UserId, Context);
                _ -> noop
            end,
            z_render:wire({redirect, [ {location, m_rsc:page_url(UserId, Context)} ]}, Context);
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to do this.", Context), Context)
    end;
event(#submit{ message={update_status, Args} }, Context) ->
    case z_acl:is_allowed(use, mod_payment, Context) orelse z_acl:is_admin(Context) of
        true ->
            {payment_id, PaymentId} = proplists:lookup(payment_id, Args),
            NewStatus = z_context:get_q(<<"status">>, Context),
            set_payment_status(PaymentId, NewStatus, Context),
            ?zInfo("Payment ~p manually changed to '~s'", [ PaymentId, NewStatus ], Context),
            z_render:wire({reload, []}, Context);
        false ->
            z_render:growl_error(?__("You do not have permission to change the status", Context), Context)
    end;
event(#submit{ message={find_payment, _Args} }, Context) ->
    case z_acl:is_allowed(use, mod_payment, Context) orelse z_acl:is_admin(Context) of
        true ->
            Query = z_string:trim(z_convert:to_binary(z_context:get_q(<<"payment_search">>, Context))),
            case find_payment(Query, Context) of
                {ok, #{ <<"payment_nr">> := PaymentNr }} ->
                    z_render:dialog(
                        ?__("Payment", Context),
                        "_dialog_payment_info.tpl",
                        [ {payment_nr, PaymentNr} ],
                        Context);
                {error, _} ->
                    z_render:growl_error(?__("No payment found.", Context), Context)
            end;
        false ->
            z_render:growl_error(?__("You do not have permission to view payments.", Context), Context)
    end;
event(#postback{ message={sync_pending, _} }, Context) ->
    case z_acl:is_allowed(use, mod_payment, Context) orelse z_acl:is_admin(Context) of
        true ->
            sync_pending(Context),
            z_render:growl(?__("Checking status for pending and new transactions, come back later.", Context), Context);
        false ->
            z_render:growl_error(?__("You do not have permission to change the status", Context), Context)
    end.

is_allowed(UserId, Context) ->
    UserId =:= z_acl:user(Context)
    orelse z_acl:is_admin(Context)
    orelse z_acl:is_allowed(use, mod_payment, Context).

-spec find_payment(Query, Context) -> Result
    when
        Query :: binary(),
        Context :: z:context(),
        Result :: {ok, map()} | {error, term()}.
find_payment(<<>>, _Context) ->
    {error, notfound};
find_payment(Query, Context) ->
    case z_utils:only_digits(Query) of
        true ->
            case m_payment:get(binary_to_integer(Query), Context) of
                {ok, _Payment} = OK ->
                    OK;
                {error, _} ->
                    m_payment:get(Query, Context)
            end;
        false ->
            m_payment:get(Query, Context)
    end.

%% @doc Extract a payment request from the arguments, with a fallback to the query
%% arguments.
payment_request_from_query(Key, UserId, Args, Context) ->
    Recurring = case proplists:get_value(is_recurring_start, Args) of
        undefined -> z_convert:to_bool( z_context:get_q(<<"is_recurring_start">>, Context) );
        R -> z_convert:to_bool(R)
    end,
    Amount = case proplists:get_value(amount, Args) of
        undefined -> z_convert:to_float(z_context:get_q(<<"amount">>, Context));
        ArgAmount -> ArgAmount
    end,
    Currency = case proplists:get_value(currency, Args) of
        undefined ->
            case currency( z_context:get_q(<<"currency">>, Context) ) of
                <<>> -> m_payment:default_currency(Context);
                QCurrency -> QCurrency
            end;
        ArgCurrency -> ArgCurrency
    end,
    DefaultDescription = m_payment:default_description(Context),
    Description = case proplists:get_value(description, Args) of
        undefined ->
            case z_context:get_q(<<"description">>, Context) of
                <<>> -> proplists:get_value(default_description, Args, DefaultDescription);
                undefined -> proplists:get_value(default_description, Args, DefaultDescription);
                Desc -> z_convert:to_binary(Desc)
            end;
        Desc ->
            z_convert:to_binary(Desc)
    end,
    Description1 = sanitize_description(Description),
    DescriptionRef = case z_context:get_q(<<"reference">>, Context) of
        undefined ->
            Description1;
        Ref when is_binary(Ref) ->
            case valid_reference(Ref) of
                {true, <<>>} -> Description1;
                {true, Ref1} when Description1 =:= <<>> -> Ref1;
                {true, Ref1} -> <<Description1/binary, " (", Ref1/binary, ")">>;
                false -> Description1
            end
    end,
    Cols = z_db:column_names(payment, Context),
    ExtraProps = lists:filter(
        fun
            ({key, _}) -> false;
            ({amount, _}) -> false;
            ({currency, _}) -> false;
            ({user_id, _}) -> false;
            ({is_recurring_start, _}) -> false;
            ({description, _}) -> false;
            ({default_description, _}) -> false;
            ({is_paid, _}) -> false;
            ({is_failed, _}) -> false;
            ({is_payment_link, true}) -> true;
            ({is_payment_link, _}) -> false;
            ({K, _}) -> is_allowed_arg(K, Cols)
        end,
        Args),
    #payment_request{
        key = z_convert:to_binary(Key),
        user_id = UserId,
        amount = Amount,
        currency = Currency,
        language = z_context:language(Context),
        description_html = z_html:escape(DescriptionRef),
        description = DescriptionRef,
        is_qargs = true,
        is_recurring_start = Recurring,
        extra_props = ExtraProps
    }.

valid_reference(Ref) when is_binary(Ref) ->
    Ref1 = z_string:trim(Ref),
    Ref2 = z_string:sanitize_utf8(Ref1),
    case Ref2 of
        <<>> -> {true, <<>>};
        R when size(R) > ?MAX_REFERENCE_LENGTH -> {false, <<>>};
        R -> {true, R}
    end.

sanitize_description(Description) ->
    Desc1 = z_string:trim(Description),
    Desc2 = z_string:sanitize_utf8(Desc1),
    Desc3 = z_string:trim(Desc2),
    z_string:truncate(Desc3, ?MAX_DESCRIPTION_LENGTH).

%% @doc Only allow non-payment columns or name/address like extra props.
is_allowed_arg(K, Cols) when is_atom(K) ->
    case lists:member(K, Cols) of
        true ->
            case z_convert:to_binary(K) of
                <<"name_", _/binary>> -> true;
                <<"address_", _/binary>> -> true;
                <<"email">> -> true;
                <<"phone">> -> true;
                _ -> false
            end;
        false ->
            true
    end.

-define(is_upper(C), (C >= $A andalso C =< $Z)).

currency(undefined) -> <<>>;
currency(<<>>) -> <<>>;
currency(<<"EUR">>) -> <<"EUR">>;
currency(<<"USD">>) -> <<"USD">>;
currency(<<"CAD">>) -> <<"CAD">>;
currency(<<"GBP">>) -> <<"GBP">>;
currency(<<"SEK">>) -> <<"SEK">>;
currency(<<A, B, C>>) when ?is_upper(A), ?is_upper(B), ?is_upper(C) ->
    % We might want to replace this with a list of known currencies.
    <<A, B, C>>;
currency(_) -> <<>>.


observe_search_query(#search_query{name = <<"payments">>, offsetlimit=OffsetLimit }, Context) ->
    case z_acl:is_allowed(use, mod_payment, Context) orelse z_acl:is_admin(Context) of
        true ->
            m_payment:search_query(OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
    #menu_item{id=admin_payments_overview,
               parent=admin_modules,
               label=?__("Payments", Context),
               url={payments_admin_overview, []},
               visiblecheck={acl, use, mod_payment}}
    | Acc
    ].



%% @doc Payment request - create payment and check if a payment service provider module
%%      can handle the payment request. Returns an uri for the user to finalize the payment.
observe_payment_request(#payment_request{} = Req, Context) ->
    % 1. Create a new payment record.
    % 2. Check which payment module wants to handle this
    %    2b. Update payment with PSP specific information (if any)
    % 3. Return either 'undefined' or a #payment_request_redirect{} record
    case m_payment:insert(Req, Context) of
        {ok, PaymentId} ->
            {ok, Payment} = m_payment:get(PaymentId, Context),
            PspReq = #payment_psp_request{
                payment_id = PaymentId,
                payment_nr = maps:get(<<"payment_nr">>, Payment),
                currency = maps:get(<<"currency">>, Payment),
                amount = maps:get(<<"amount">>, Payment),
                is_recurring_start = maps:get(<<"is_recurring_start">>, Payment),
                preferred_psp_module = Req#payment_request.preferred_psp_module
            },
            case psp_request(PspReq, Context) of
                {ok, #payment_psp_handler{ psp_module = PSPMod } = Handler} ->
                    ?LOG_INFO(#{
                        in => zotonic_mod_payment,
                        text => <<"Payment: insert payment">>,
                        result => ok,
                        payment_id => PaymentId,
                        psp => PSPMod
                    }),
                    ok = m_payment:update_psp_handler(PaymentId, Handler, Context),
                    #payment_request_redirect{
                        payment_id = PaymentId,
                        redirect_uri = Handler#payment_psp_handler.redirect_uri
                    };
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_payment,
                        text => <<"Payment: PSP error return value for payment">>,
                        result => error,
                        reason => Reason,
                        payment_id => PaymentId
                    }),
                    m_payment:set_payment_status(PaymentId, error, Context),
                    Error;
                undefined ->
                    % Set the payment to 'NOPSP'
                    ?LOG_ERROR(#{
                        in => zotonic_mod_payment,
                        text => <<"Payment: no PSP return value for payment">>,
                        result => error,
                        reason => no_psp,
                        payment_id => PaymentId
                    }),
                    m_payment:set_payment_status(PaymentId, error, Context),
                    {error, no_psp}
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_payment,
                text => <<"Payment: Could not insert payment">>,
                result => error,
                reason => Reason,
                payment_req => Req,
                qargs => z_context:get_q_all_noz(Context)
            }),
            Error
    end.

psp_request(PsPReq, Context) ->
    case z_notifier:first(PsPReq, Context) of
        {ok, #payment_psp_handler{} = Handler} ->
            {ok, Handler};
        {error, _Reason} = Error ->
            Error;
        undefined when PsPReq#payment_psp_request.preferred_psp_module =/= undefined ->
            PsPReq1 = PsPReq#payment_psp_request{ preferred_psp_module = undefined },
            z_notifier:first(PsPReq1, Context);
        undefined ->
            undefined
    end.


%% @doc Every day all pending and new transactions are checked for external status changes.
-spec observe_tick_24h(tick_24h, z:context()) -> ok.
observe_tick_24h(tick_24h, Context) ->
    delete_old(Context),
    sync_pending(Context),
    ok.

%% @doc Delete all payments older than the configured 'delete_after_days' number of days.
delete_old(Context) ->
    case m_config:get_value(mod_payment, delete_after_days, Context) of
        undefined ->
            ok;
        <<>> ->
            ok;
        Days ->
            m_payment:delete_old(Days, Context)
    end.

%% @doc Fetch the PSP payment status for all non finalized payments.
sync_pending(Context) ->
    ContextAsync = z_context:prune_for_async(Context),
    erlang:spawn(
        fun() ->
            {ok, AllPending} = m_payment:list_status_check(ContextAsync),
            ?LOG_INFO(#{
                in => zotonic_mod_payment,
                text => <<"Payment: checking pending payments - start">>,
                count => length(AllPending)
            }),
            lists:foreach(
                fun(#{ <<"id">> := PaymentId } = Payment) ->
                    PspSync = #payment_psp_status_sync{
                        payment_id = PaymentId,
                        psp_module = psp_module( maps:get(<<"psp_module">>, Payment) ),
                        psp_external_id = maps:get(<<"psp_external_id">>, Payment),
                        psp_data = maps:get(<<"psp_data">>, Payment)
                    },
                    case z_notifier:first(PspSync, ContextAsync) of
                        ok ->
                            ok;
                        {error, _} ->
                            maybe_set_error(Payment, ContextAsync);
                        undefined ->
                            maybe_set_error(Payment, ContextAsync)
                    end
                end,
                AllPending),
            ?LOG_INFO(#{
                in => zotonic_mod_payment,
                text => <<"Payment: checking pending payments - done">>,
                count => length(AllPending)
            })
        end).

psp_module(undefined) -> undefined;
psp_module(<<>>) -> undefined;
psp_module(Mod) when is_binary(Mod) -> binary_to_atom(Mod, utf8).

maybe_set_error(Payment, Context) ->
    OneWeekAgo = prev_day(7, calendar:universal_time()),
    LastUpdate = case maps:get(<<"status_date">>, Payment) of
        undefined -> maps:get(<<"modified">>, Payment);
        DT -> DT
    end,
    case LastUpdate < OneWeekAgo of
        true ->
            % Too old - set to error.
            PaymentId = maps:get(<<"id">>, Payment),
            ?LOG_INFO(#{
                in => zotonic_mod_payment,
                text => <<"Payment: Set payment as error due to timeout.">>,
                result => error,
                reason => timeout,
                payment_id => PaymentId
            }),
            set_payment_status(PaymentId, error, Context);
        false ->
            ok
    end.

prev_day(0, DT) -> DT;
prev_day(N, DT) when N > 0 -> prev_day( N-1, z_datetime:prev_day(DT) ).

-spec observe_export_resource_visible(#export_resource_visible{}, z:context()) -> boolean() | undefined.
observe_export_resource_visible(#export_resource_visible{dispatch = export_payments_csv}, Context) ->
    z_acl:is_allowed(use, mod_payment, Context);
observe_export_resource_visible(_, _) ->
    undefined.

-spec observe_export_resource_filename(#export_resource_filename{}, z:context()) -> {ok, binary()} | undefined.
observe_export_resource_filename(#export_resource_filename{dispatch = export_payments_csv}, Context) ->
    FormattedDate = z_datetime:format(calendar:universal_time(), "Ymd-His", Context),
    {ok, iolist_to_binary([<<"payments-">>, FormattedDate])};
observe_export_resource_filename(_, _) ->
    undefined.

%% @doc Add CSV headers
-spec observe_export_resource_header(#export_resource_header{}, z:context()) -> {ok, list()} | undefined.
observe_export_resource_header(#export_resource_header{dispatch = export_payments_csv}, _Context) ->
    {ok, payment_export:headers()};
observe_export_resource_header(_, _) ->
    undefined.

observe_export_resource_data(#export_resource_data{dispatch = export_payments_csv}, Context) ->
    payment_export:data(Context);
observe_export_resource_data(_, _) ->
    undefined.

-spec observe_export_resource_encode(#export_resource_encode{}, z:context()) -> {ok, binary()} | undefined.
observe_export_resource_encode(#export_resource_encode{dispatch = export_payments_csv, data = Item}, Context) ->
    Values = payment_export:values(Item, Context),
    {ok, export_encode_csv:encode(Values, Context)};
observe_export_resource_encode(_, _) ->
    undefined.


%% @doc Called by a PSP, set the status of a payment. This also broadcasts success or failure for the payment.
-spec set_payment_status(integer(), atom()|binary()|list(), z:context()) -> ok | {error, term()}.
set_payment_status(PaymentId, Status, Context) ->
    set_payment_status(PaymentId, Status, calendar:universal_time(), Context).

-spec set_payment_status(integer(), atom()|binary()|list(), calendar:datetime(), z:context()) -> ok | {error, term()}.
set_payment_status(PaymentId, Status, DT, Context) when is_integer(PaymentId), is_binary(Status) ->
    set_payment_status(PaymentId, binary_to_existing_atom(Status, utf8), DT, Context);
set_payment_status(PaymentId, Status, DT, Context) when is_integer(PaymentId), is_list(Status) ->
    set_payment_status(PaymentId, list_to_existing_atom(Status), DT, Context);
set_payment_status(PaymentId, Status, DT, Context) when is_integer(PaymentId), is_atom(Status) ->
    validate_payment_status(Status),
    case m_payment:set_payment_status(PaymentId, Status, DT, Context) of
        {ok, changed} ->
            % Status is the new payment status
            {ok, Payment} = m_payment:get(PaymentId, Context),
            maybe_send_email(Status, Payment, Context),
            z_notifier:notify(
                #payment_status{
                    key = maps:get(<<"key">>, Payment),
                    payment_id = PaymentId,
                    user_id = maps:get(<<"user_id">>, Payment),
                    is_paid = maps:get(<<"is_paid">>, Payment, false),
                    is_failed = maps:get(<<"is_failed">>, Payment, false),
                    is_recurring_payment = is_integer( maps:get(<<"recurring_payment_id">>, Payment) ),
                    status = maps:get(<<"status">>, Payment),
                    date = maps:get(<<"status_date">>, Payment)
                },
                Context),
            ok;
        {ok, unchanged} ->
            ok;
        {error, _} = Error ->
            Error
    end.

%% @doc Crash if not valid payment status.
validate_payment_status(new) -> true;
validate_payment_status(pending) -> true;
validate_payment_status(paid) -> true;
validate_payment_status(cancelled) -> true;
validate_payment_status(failed) -> true;
validate_payment_status(refunded) -> true;
validate_payment_status(error) -> true.


%% @doc Maybe sent a "paid" email.
maybe_send_email(paid, Payment, Context) ->
    EmailPaid = z_convert:to_binary(m_config:get_value(mod_payment, email_paid, Context)),
    Es = z_email_utils:extract_emails(EmailPaid),
    lists:foreach(
        fun(E) ->
            Vs = [
                {status, paid},
                {payment, Payment}
            ],
            z_email:send_render(E, "_email_payment_paid.tpl", Vs, Context)
        end,
        Es);
maybe_send_email(_Status, _Payment, _Context) ->
    ok.


%% @doc Install the payment and payment log tables.
manage_schema(_Version, Context) ->
    ok = m_payment:install(Context),
    ok = m_payment_log:install(Context).
