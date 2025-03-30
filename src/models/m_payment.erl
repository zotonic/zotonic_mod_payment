%% @copyright 2018-2024 Driebit BV
%% @doc Main payment model and SQL definitions. Maintains a single table of all
%% payments. All PSP modules store their payments in this table, including extra
%% PSP specific properties.
%% @end

%% Copyright 2018-2024 Driebit BV
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

-module(m_payment).

-export([
    m_get/3,

    default_description/1,
    default_currency/1,
    default_amount/1,

    list_user/2,

    insert/2,
    insert_recurring_payment/4,
    insert_recurring_payment/5,
    get/2,
    get_by_psp/3,
    update_psp_handler/3,
    payment_psp_view_url/2,
    set_payment_status/3,
    set_payment_status/4,
    cancel_recurring_payment/2,

    search_query/2,
    list_status_check/1,

    delete_old/2,

    install/1,

    delete_all_payments/1,
    indices/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../../include/payment.hrl").

m_get([ <<"default">>, <<"currency">> | Rest ], _Msg, Context) ->
    {ok, {default_currency(Context), Rest}};
m_get([ <<"default">>, <<"amount">> | Rest ], _Msg, Context) ->
    {ok, {default_amount(Context), Rest}};
m_get([ <<"default">>, <<"description">> | Rest ], _Msg, Context) ->
    {ok, {default_description(Context), Rest}};
m_get([ <<"redirect_psp">>, PaymentNr | Rest ], _Msg, Context) when is_binary(PaymentNr) ->
    case payment_psp_view_url(PaymentNr, Context) of
        {ok, Url} ->
            {ok, {Url, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"list_user">>, User | Rest ], _Msg, Context) ->
    UserId = m_rsc:rid(User, Context),
    case z_acl:is_allowed(use, mod_payment, Context)
        orelse z_acl:is_admin(Context)
        orelse z_acl:rsc_editable(UserId, Context)
    of
        true ->
            {ok, {list_user(UserId, Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"status">>, PaymentNr | Rest ], _Msg, Context) when is_binary(PaymentNr) ->
    case get(PaymentNr, Context) of
        {ok, #{ <<"user_id">> := UserId } = Payment} ->
            case z_acl:is_allowed(use, mod_payment, Context)
                orelse UserId =:= undefined
                orelse UserId =:= z_acl:user(Context)
                orelse z_acl:rsc_editable(UserId, Context)
            of
                true ->
                    Status = #{
                        <<"is_paid">> => maps:get(<<"is_paid">>, Payment),
                        <<"is_failed">> => maps:get(<<"is_failed">>, Payment),
                        <<"status">> => maps:get(<<"status">>, Payment)
                    },
                    {ok, {Status, Rest}};
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end;
m_get([ PaymentNr | Rest ], _Msg, Context) when is_binary(PaymentNr) ->
    case get(PaymentNr, Context) of
        {ok, #{ <<"user_id">> := UserId } = Payment} ->
            case z_acl:is_allowed(use, mod_payment, Context)
                orelse UserId =:= z_acl:user(Context)
                orelse z_acl:rsc_editable(UserId, Context)
            of
                true ->
                    {ok, {Payment, Rest}};
                false ->
                    {error, eacces}
            end;
        {error, _} = Error ->
            Error
    end.

-spec default_description( z:context() ) -> binary().
default_description(Context) ->
    z_convert:to_binary( m_config:get_value(mod_payment, description, Context) ).

-spec default_currency( z:context() ) -> binary().
default_currency(Context) ->
    case m_config:get_value(mod_payment, currency, Context) of
        undefined -> ?PAYMENT_CURRENCY_DEFAULT;
        <<>> -> ?PAYMENT_CURRENCY_DEFAULT;
        Currency -> Currency
    end.

-spec default_amount( z:context() ) -> integer() | undefined.
default_amount(Context) ->
    case m_config:get_value(mod_payment, amount, Context) of
        undefined -> undefined;
        <<>> -> undefined;
        Amount -> z_convert:to_integer(Amount)
    end.


%% @doc Fetch all payments of an user, newest first
-spec list_user(UserId, Context) -> Payments
    when UserId :: m_rsc:resource_id(),
         Context :: z:context(),
         Payments :: [ map() ].
list_user(UserId, Context) ->
    {ok, L} = z_db:qmap("
        select *
        from payment
        where user_id = $1
        order by created desc",
        [UserId],
        Context),
    lists:map(
        fun(P) ->
            add_status_flags(P)
        end,
        L).

%% @doc Create new payment.
-spec insert(PaymentReq, Context) -> {ok, PaymentId} | {error, term()}
    when PaymentReq :: #payment_request{},
         Context :: z:context(),
         PaymentId :: integer().
insert(PaymentReq, Context) ->
    UserId = case PaymentReq#payment_request.user_id of
        undefined -> z_acl:user(Context);
        UId when is_integer(UId) -> UId
    end,
    PaymentNr = z_ids:identifier(32),
    DescrHTML = case PaymentReq#payment_request.description_html of
        undefined -> <<>>;
        RDHtml -> z_sanitize:html(RDHtml, Context)
    end,
    Descr = case PaymentReq#payment_request.description of
        undefined -> z_string:trim(z_html:strip(DescrHTML));
        RD -> RD
    end,
    Payment = #{
        <<"user_id">> => UserId,
        <<"payment_nr">> => PaymentNr,
        <<"is_recurring_start">> => PaymentReq#payment_request.is_recurring_start,
        <<"key">> => PaymentReq#payment_request.key,
        <<"currency">> => PaymentReq#payment_request.currency,
        <<"amount">> => PaymentReq#payment_request.amount,
        <<"description">> => z_string:truncate(Descr, 60),
        <<"description_html">> => DescrHTML,
        <<"language">> => language(UserId, Context)
    },
    Payment1 = maps:merge(Payment, naw_props(UserId, PaymentReq#payment_request.is_qargs, Context)),
    Payment2 = maps:merge(Payment1, extra_props(PaymentReq#payment_request.extra_props, PaymentReq#payment_request.is_qargs, Context)),
    Payment3 = maps:merge(qargs_props(PaymentReq#payment_request.is_qargs, Context), Payment2),
    case validate_payment(Payment3) of
        ok ->
            z_db:insert(payment, Payment3, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Insert a recurring payment, automatically paid via the PSP. The RecurringPaymentId
%% refers to the recurring payment that started the subscription.
-spec insert_recurring_payment(PaymentId, Currency, Amount, Context) -> {ok, NewPaymentId} | {error, term()}
    when PaymentId :: integer(),
         Currency :: binary() | string() | undefined,
         Amount :: number(),
         Context :: z:context(),
         NewPaymentId :: integer().
insert_recurring_payment(RecurringPaymentId, Currency, Amount, Context) ->
    insert_recurring_payment(RecurringPaymentId, erlang:universaltime(), Currency, Amount, Context).

-spec insert_recurring_payment(PaymentId, Created, Currency, Amount, Context) -> {ok, NewPaymentId} | {error, term()}
    when PaymentId :: integer(),
         Created :: calendar:datetime(),
         Currency :: binary() | string() | undefined,
         Amount :: number(),
         Context :: z:context(),
         NewPaymentId :: integer().
insert_recurring_payment(RecurringPaymentId, Created, Currency, Amount, Context) ->
    case ?MODULE:get(RecurringPaymentId, Context) of
        {ok, #{
            <<"psp_module">> := PspModule,
            <<"user_id">> := UserId,
            <<"key">> := Key,
            <<"description">> := Descr,
            <<"description_html">> := DescrHTML,
            <<"name_first">> := NameFirst,
            <<"name_surname_prefix">> := NamePrefix,
            <<"name_surname">> := NameSurname,
            <<"address_street_1">> := Street1,
            <<"address_street_2">> := Street2,
            <<"address_postcode">> := Postcode,
            <<"address_city">> := City,
            <<"address_state">> := State,
            <<"address_country">> := Country,
            <<"email">> := Email,
            <<"phone">> := Phone
        } = Payment} ->
            Currency1 = case Currency of
                undefined -> maps:get(<<"currency">>, Payment);
                _ -> Currency
            end,
            NewPayment = #{
                <<"payment_nr">> => z_ids:identifier(32),
                <<"recurring_payment_id">> => RecurringPaymentId,
                <<"psp_module">> => PspModule,
                <<"user_id">> => UserId,
                <<"is_recurring_start">> => false,
                <<"key">> => Key,
                <<"currency">> => Currency1,
                <<"amount">> => z_convert:to_float(Amount),
                <<"description">> => Descr,
                <<"description_html">> => DescrHTML,
                <<"name_first">> => NameFirst,
                <<"name_surname_prefix">> => NamePrefix,
                <<"name_surname">> => NameSurname,
                <<"address_street_1">> => Street1,
                <<"address_street_2">> => Street2,
                <<"address_postcode">> => Postcode,
                <<"address_city">> => City,
                <<"address_state">> => State,
                <<"address_country">> => Country,
                <<"email">> => Email,
                <<"phone">> => Phone,
                <<"created">> => Created
            },
            case validate_payment(NewPayment) of
                ok ->
                    z_db:insert(payment, NewPayment, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

% Extra properties
-spec extra_props( list(), boolean(), z:context() ) -> map().
extra_props(Props, false, _Context) ->
    Props1 = lists:filtermap(
        fun
            ({_, undefined}) -> false;
            ({_, <<>>}) -> false;
            ({_, ""}) -> false;
            ({K, V}) -> {z_string:to_name(K), V};
            (_) -> false
        end,
        Props),
    maps:from_list(Props1);
extra_props(Props, true, Context) ->
    Props1 = lists:map(
        fun
            ({K, _} = KV) ->
                case z_context:get_q(K, Context) of
                    undefined -> KV;
                    V -> {K, V}
                end;
            (K) when is_atom(K); is_binary(K) ->
                K1 = z_string:to_name(K),
                {K1, z_context:get_q(K1, Context)}
        end,
        Props),
    extra_props(Props1, false, Context).

% Properties from the posted form
-spec qargs_props(boolean(), z:context()) -> map().
qargs_props(false, _Context) ->
    #{};
qargs_props(true, Context) ->
    lists:foldl(
        fun
            ({K, V}, Acc) when is_binary(K), is_binary(V) ->
                Acc#{ z_string:to_name(K) => V };
            (_, Acc) ->
                Acc
        end,
        #{},
        z_context:get_q_all_noz(Context)).


% We should have an email address, or an user-id, and name + phone no.
validate_payment(Payment) ->
    Validations = [
        {<<"amount">>,  fun() ->
                            is_number(maps:get(<<"amount">>, Payment, undefined))
                            andalso maps:get(<<"amount">>, Payment) > 0
                         end},
        {<<"email">>,   fun() ->
                            is_email_address(maps:get(<<"email">>, Payment, undefined))
                        end},
        {<<"contact">>, fun() ->
                            is_integer(maps:get(<<"user_id">>, Payment, undefined))
                            orelse (not z_utils:is_empty(maps:get(<<"name_surname">>, Payment, undefined)))
                        end}
    ],
    check(Validations).

check([]) ->
    ok;
check([ {Reason, F} | Vs ]) ->
    case F() of
        true -> check(Vs);
        false -> {error, Reason}
    end.

is_email_address(undefined) ->
    false;
is_email_address(Email) ->
    z_email_utils:is_email(Email).

naw_props(UserId, IsQArgs, Context) ->
    Naw = [
        {P, p(UserId, P, IsQArgs, Context)}
        || P <- [
            <<"name_first">>,
            <<"name_surname_prefix">>,
            <<"name_surname">>,
            <<"address_street_1">>,
            <<"address_street_2">>,
            <<"address_city">>,
            <<"address_state">>,
            <<"address_country">>,
            <<"address_postcode">>,
            <<"email">>,
            <<"phone">>
        ]
    ],
    maps:from_list( lists:filter( fun({_,V}) -> V =/= undefined end, Naw ) ).

p(Id, Prop, true, Context) ->
    case z_context:get_q(Prop, Context) of
        undefined ->
            p(Id, Prop, false, Context);
        V ->
            V1 = z_html:escape_check( z_string:trim(V) ),
            z_string:truncate(V1, 200, <<>>)
    end;
p(Id, Prop, false, Context) ->
    case m_rsc:p_no_acl(Id, Prop, Context) of
        undefined -> undefined;
        V -> z_string:truncate(z_string:trim(V), 200, <<>>)
    end.

language(undefined, Context) ->
    z_context:language(Context);
language(UserId, Context) ->
    case m_rsc:p(UserId, <<"pref_language">>, Context) of
        undefined -> z_context:language(Context);
        Lang -> Lang
    end.

-spec get(integer()|binary()|string(), z:context()) -> {ok, map()} | {error, term()}.
get(PaymentId, Context) when is_integer(PaymentId) ->
    case z_db:qmap_row(
        "select * from payment where id = $1",
        [PaymentId],
        Context)
    of
        {ok, Props} -> {ok, add_status_flags(Props)};
        {error, _} = Error -> Error
    end;
get(PaymentNr, Context) when is_binary(PaymentNr); is_list(PaymentNr) ->
    case z_db:qmap_row(
        "select * from payment where payment_nr = $1",
        [PaymentNr],
        Context)
    of
        {ok, Props} -> {ok, add_status_flags(Props)};
        {error, _} = Error -> Error
    end.

-spec get_by_psp(atom(), binary()|string(), z:context()) -> {ok, map()} | {error, term()}.
get_by_psp(_Module, <<>>, _Context) ->
    {error, notfound};
get_by_psp(_Module, "", _Context) ->
    {error, notfound};
get_by_psp(_Module, undefined, _Context) ->
    {error, notfound};
get_by_psp(PspModule, PspExternalId, Context) ->
    case z_db:qmap_row(
        "select * from payment where psp_module = $1 and psp_external_id = $2",
        [PspModule, PspExternalId],
        Context)
    of
        {ok, Props} -> {ok, add_status_flags(Props)};
        {error, _} = Error -> Error
    end.

add_status_flags(#{ <<"status">> := StatusBin, <<"psp_module">> := Psp } = Payment) ->
    Status = bin_to_atom(StatusBin),
    Payment#{
        <<"status">> => Status,
        <<"is_paid">> => is_paid_status(Status),
        <<"is_failed">> => is_failed_status(Status),
        <<"psp_module">> => z_convert:to_atom(Psp)
    }.

bin_to_atom(undefined) -> undefined;
bin_to_atom(A) when is_atom(A) -> A;
bin_to_atom(B) when is_binary(B) -> erlang:binary_to_atom(B, utf8).

is_paid_status(paid) -> true;
is_paid_status(_) -> false.

is_failed_status(error) -> true;
is_failed_status(failed) -> true;
is_failed_status(cancelled) -> true;
is_failed_status(refunded) -> true;
is_failed_status(_) -> false.


-spec update_psp_handler(integer(), #payment_psp_handler{}, z:context()) -> ok | {error, term()}.
update_psp_handler(PaymentId, Handler, Context) ->
    case z_db:q("
        update payment
        set psp_module = $2,
            psp_external_id = $3,
            psp_payment_description = $4,
            psp_data = $5
        where id = $1
        ",
        [
            PaymentId,
            Handler#payment_psp_handler.psp_module,
            Handler#payment_psp_handler.psp_external_id,
            Handler#payment_psp_handler.psp_payment_description,
            ?DB_PROPS(Handler#payment_psp_handler.psp_data)
        ],
        Context)
    of
        1 -> ok;
        0 -> {error, notfound}
    end.

-spec set_payment_status(integer(), atom(), z:context()) -> {ok, changed|unchanged} | {error, term()}.
set_payment_status(PaymentId, Status, Context) ->
    Now = calendar:universal_time(),
    set_payment_status(PaymentId, Status, Now, Context).

-spec set_payment_status(integer(), atom(), calendar:datetime(), z:context()) ->
    {ok, changed|unchanged} | {error, term()}.
set_payment_status(PaymentId, Status, StatusDate, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            CurrStatusBin = z_db:q1("
                select status
                from payment
                where id = $1",
                [PaymentId],
                Ctx),
            case z_convert:to_atom(CurrStatusBin) of
                undefined ->
                    {error, notfound};
                CurrStatus when CurrStatus =:= Status ->
                    {ok, unchanged};
                _OldStatus ->
                    case z_db:q("
                        update payment
                        set status = $1::varchar,
                            status_date = $3
                        where id = $2
                          and status <> $1::varchar
                          and (   status_date is null
                               or status_date <= $3)",
                        [z_convert:to_binary(Status), PaymentId, StatusDate],
                        Ctx)
                    of
                        0 -> {ok, unchanged};
                        1 -> {ok, changed}
                    end
            end
        end,
        Context).

search_query({Offset, Limit}, Context) ->
    {ok, Rows} = z_db:qmap("
        select *
        from payment
        order by created desc
        offset $1
        limit $2",
        [Offset-1, Limit],
        Context),
    #search_result{
        result = Rows,
        total = total(Context)
    }.

total(Context) ->
    z_db:q1("select count(*) from payment", Context).


%% @doc Return a list of all payments that are in a temporary status for longer
%%      than an hour.
-spec list_status_check( z:context() ) -> {ok, list( map() )} | {error, term()}.
list_status_check(Context) ->
    LastHour = z_datetime:prev_hour( calendar:universal_time() ),
    z_db:qmap("
            select *
            from payment
            where status in ('new', 'pending')
              and (   (status_date is null and created < $1)
                   or status_date < $1)
            order by id
        ",
        [ LastHour ],
        Context).

-spec payment_psp_view_url(binary()|integer(), z:context()) -> {ok, binary()} | {error, term()}.
payment_psp_view_url(PaymentId, Context) ->
    case get(PaymentId, Context) of
        {ok, #{
            <<"id">> := Id,
            <<"psp_module">> := PspModule,
            <<"psp_external_id">> := PspExternalId,
            <<"psp_data">> := PspData
        }} ->
            Req = #payment_psp_view_url{
                payment_id = Id,
                psp_module = PspModule,
                psp_external_id = PspExternalId,
                psp_data = PspData
            },
            case z_notifier:first(Req, Context) of
                undefined -> {error, unsupported};
                {ok, Uri} -> {ok, Uri}
            end;
        {error, _} = Error ->
            Error
    end.


%% @doc Delete payments that are unmodified in the last N days. This is useful to prune the
%% payments tables from personal information.
-spec delete_old(Days, Context) -> {ok, integer()}
    when Days :: integer(),
         Context :: z:context().
delete_old(Days, Context) when is_integer(Days), Days > 1 ->
    N = z_db:q("
        delete from payment
        where modified < $1
          and (is_recurring_start = false or status <> 'paid')",
        [ z_datetime:to_datetime(z_datetime:timestamp() - 24*3600*Days) ],
        Context),
    {ok, N};
delete_old(_Days, _Context) ->
    {ok, 0}.

-spec delete_all_payments(z:context()) -> ok.
delete_all_payments(Context) ->
    z_db:q("delete from payment_log", Context),
    z_db:q("delete from payment", Context),
    ok.

-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(payment, Context) of
        false ->
            [] = z_db:q("
                create table payment (
                    id serial not null,
                    user_id int,
                    payment_nr character varying(64) not null,
                    status character varying(16) not null default 'new',
                    status_date timestamp,
                    is_recurring_start boolean not null default false,
                    recurring_payment_id int,

                    psp_module character varying(64),
                    psp_external_id character varying(128),
                    psp_payment_description text,
                    psp_data bytea,

                    key character varying(256),
                    language character varying(16) not null default 'en',
                    description character varying(64) not null default '',
                    description_html text not null default '',

                    name_first character varying(256),
                    name_surname_prefix character varying(256),
                    name_surname character varying(256),
                    address_street_1 character varying(256),
                    address_street_2  character varying(256),
                    address_postcode character varying(256),
                    address_city character varying(256),
                    address_state character varying(256),
                    address_country character varying(8),
                    email character varying(256),
                    phone character varying(256),

                    props bytea,

                    currency character varying(10) not null default 'EUR',
                    amount float not null,

                    created timestamp with time zone NOT NULL DEFAULT now(),
                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    constraint payment_pkey primary key (id),
                    constraint payment_payment_nr unique (payment_nr),
                    constraint payment_user_id
                        foreign key (user_id)
                        references rsc (id)
                        on update cascade on delete set null
                )", Context),
            [] = z_db:q("
                create index payment_created_key
                on payment (created)",
                Context),
            [] = z_db:q("
                create index fki_payment_user_id
                on payment (user_id)",
                Context),
            [] = z_db:q("
                create index payment_status_key
                on payment (status)",
                Context),
            [] = z_db:q("
                create index payment_psp_external_id_key
                on payment (psp_external_id)",
                Context),
            [] = z_db:q("
                create index fki_payment_recurring_payment_id
                on payment (recurring_payment_id)",
                Context),
            [] = z_db:q("
                ALTER TABLE payment
                ADD CONSTRAINT fk_payment_recurring_payment_id FOREIGN KEY (recurring_payment_id)
                REFERENCES payment (id)
                ON UPDATE CASCADE ON DELETE RESTRICT",
                Context),
            ok;
        true ->
            add_recurring_payment_id_column(Context),
            add_recurring_column(Context),
            add_status_date_column(Context),
            case lists:member(<<"payment_created_key">>, indices("payment", Context)) of
                true ->
                    ok;
                false ->
                    [] = z_db:q("
                        create index payment_created_key
                        on payment (created)",
                        Context)
            end,
            case z_db:column(payment, psp_external_id, Context) of
                {ok, #column_def{ length = L }} when L < 128 ->
                    [] = z_db:q("
                        alter table payment
                        alter column psp_external_id type character varying(128)
                        ", Context),
                    z_db:flush(Context);
                {ok, _} ->
                    ok
            end,
            ok
    end.

indices(Table, Context) ->
    Is = z_db:q("
        select i.relname
        from pg_class t,
             pg_class i,
             pg_index ix,
             pg_attribute a
        where t.oid = ix.indrelid
          and i.oid = ix.indexrelid
          and a.attrelid = t.oid
          and a.attnum = ANY(ix.indkey)
          and t.relkind = 'r'
          and t.relname = $1
        ", [ Table ], Context),
    [ I || {I} <- Is ].


%% @doc Add recurring_payment_id column if it was not yet present
add_recurring_payment_id_column(Context) ->
    case lists:member(recurring_payment_id, z_db:column_names(payment, Context)) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                ALTER TABLE payment
                ADD COLUMN recurring_payment_id int
                ", Context),
            [] = z_db:q("
                create index fki_payment_recurring_payment_id
                on payment (recurring_payment_id)",
                Context),
            [] = z_db:q("
                ALTER TABLE payment
                ADD CONSTRAINT fk_payment_recurring_payment_id FOREIGN KEY (recurring_payment_id)
                REFERENCES payment (id)
                ON UPDATE CASCADE ON DELETE RESTRICT",
                Context),
            z_db:flush(Context),
            ok
    end.


%% @doc Add recurring column if it was not yet present
add_recurring_column(Context) ->
    case lists:member(is_recurring_start, z_db:column_names(payment, Context)) of
        true ->
            ok;
        false ->
            case lists:member(recurring, z_db:column_names(payment, Context)) of
                true ->
                    [] = z_db:q("
                        ALTER TABLE payment
                        RENAME COLUMN recurring TO is_recurring_start
                        ", Context),
                    z_db:flush(Context),
                    ok;
                false ->
                    [] = z_db:q("
                        ALTER TABLE payment
                        ADD COLUMN is_recurring_start boolean not null default false
                        ", Context),
                    z_db:flush(Context),
                    ok
            end
    end.


%% @doc Add status_date column if it was not yet present
add_status_date_column(Context) ->
    case lists:member(status_date, z_db:column_names(payment, Context)) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                ALTER TABLE payment
                ADD COLUMN status_date timestamp with time zone
                ", Context),
            z_db:flush(Context),
            ok
    end.


cancel_recurring_payment(UserId, Context) ->
    z_db:q("update payment set is_recurring_start = false where user_id = $1", [UserId], Context).
