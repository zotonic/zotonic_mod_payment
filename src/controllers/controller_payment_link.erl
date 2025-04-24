%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Redirect to the payment provider. Accept optional description and amount.
%% @end

%% Copyright 2025 Marc Worrell
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


-module(controller_payment_link).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../../include/payment.hrl").


resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    Key = undefined,
    UserId = z_acl:user(Context2),
    Args = [
        {amount, z_context:get(description, Context2)},
        {currency, z_context:get(currency, Context2)},
        {is_recurring_start, z_context:get(is_recurring_start, Context2)},
        {description, z_context:get(description, Context2)},
        {is_payment_link, true}
    ],
    PaymentRequest = mod_payment:payment_request_from_query(Key, UserId, Args, Context2),
    case z_notifier:first(PaymentRequest, Context2) of
        #payment_request_redirect{ redirect_uri = RedirectUri } ->
            {{true, RedirectUri}, Context2};
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_payment,
                text => <<"Error creating payment from arguments">>,
                result => error,
                reason => Reason,
                args => Args,
                qargs => z_context:get_q_all(Context2)
            }),
            {{halt, 400}, Context2};
        undefined ->
            ?LOG_ERROR(#{
                in => zotonic_mod_payment,
                text => <<"Error creating payment from arguments">>,
                result => error,
                reason => no_handler,
                args => Args,
                qargs => z_context:get_q_all(Context2)
            }),
            {{halt, 400}, Context2}
    end.
