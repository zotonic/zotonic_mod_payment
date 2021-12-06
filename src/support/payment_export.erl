%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2021 Driebit BV
%% @doc Support functions for exporting payments

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

-module(payment_export).

-export([
    headers/0,
    data/1,
    values/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Get human-readable CSV headers that will be appended to exported CSV
headers() ->
    [
        date,
        status,
        description,
        currency,
        amount,
        first,
        surname,
        email,
        phone,
        street,
        city,
        postcode,
        state,
        country
    ].

data(Context) ->
    #search_result{result = Result} = z_search:search(payments, {1, 100000}, Context),
    {ok, Result}.

values(Item, Context) ->
    [ z_datetime:format(proplists:get_value(created, Item), "c", Context)
    , maps:get(<<"status">>, Item)
    , maps:get(<<"description">>, Item)
    , maps:get(<<"currency">>, Item)
    , maps:get(<<"amount">>, Item)
    , maps:get(<<"name_first">>, Item)
    , maps:get(<<"name_surname">>, Item)
    , maps:get(<<"email">>, Item)
    , maps:get(<<"phone">>, Item)
    , maps:get(<<"address_street_1">>, Item)
    , maps:get(<<"address_city">>, Item)
    , maps:get(<<"address_postcode">>, Item)
    , maps:get(<<"address_state">>, Item)
    , country( maps:get(<<"address_country">>, Item), Context )
    ].

country(undefined, _Context) ->
    <<>>;
country(Iso, Context) ->
    m_l10n:country_name(Iso, en, Context).
