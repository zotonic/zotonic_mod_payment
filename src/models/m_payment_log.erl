%% @copyright 2018-2021 Driebit BV
%% @doc Logging of events per payment.

%% Copyright 2018-2021 Driebit BV
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

-module(m_payment_log).

-export([
    log/4,
    install/1
    ]).

-spec log(PaymentId, Event, Props, Context) -> {ok, integer()}
    when PaymentId :: integer(),
         Event :: binary() | atom() | string(),
         Props :: map(),
         Context :: z:context().
log(PaymentId, Event, Props, Context) ->
    InsertProps = Props#{
        <<"payment_id">> => PaymentId,
        <<"event">> => event(Event)
    },
    z_db:insert(payment_log, InsertProps, Context).


event(Event) ->
    z_string:to_lower(z_string:trim(Event)).


-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(payment_log, Context) of
        false ->
            [] = z_db:q("
                create table payment_log (
                    id serial not null,
                    payment_id int not null,

                    event character varying(32) not null,

                    psp_module character varying(64) not null,
                    psp_external_log_id character varying(64),

                    description character varying(120),
                    props bytea,

                    actor character varying(128),

                    creator_id int,
                    created timestamp with time zone NOT NULL DEFAULT now(),

                    constraint payment_log_pkey primary key (id),
                    constraint fk_payment_log_payment_id
                        foreign key (payment_id)
                        references payment (id)
                        on update cascade on delete cascade,
                    constraint fk_payment_log_creator_id
                        foreign key (creator_id)
                        references rsc (id)
                        on update cascade on delete set null
                )", Context),
            [] = z_db:q("
                create index fki_payment_log_payment_id
                on payment_log (payment_id)",
                Context),
            [] = z_db:q("
                create index fki_payment_log_creator_id
                on payment_log (creator_id)",
                Context),
            [] = z_db:q("
                create index payment_log_psp_external_log_id_key
                on payment_log (psp_external_log_id)",
                Context),
            ok;
        true ->
            ok
    end.

