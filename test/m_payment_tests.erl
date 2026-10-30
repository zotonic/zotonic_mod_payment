%% @copyright 2026 Marc Worrell
%% @doc Regression tests for payment search filters.
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

-module(m_payment_tests).
-include_lib("eunit/include/eunit.hrl").

reference_prefix_filter_test() ->
    ?assertEqual(
        {<<" where reference like $1 escape '!'">>, [<<"donation-%">>]},
        m_payment:search_filter(#{<<"reference">> => <<"donation-">>})),
    ?assertEqual(
        {<<" where reference like $1 escape '!'">>, [<<"campaign!_100!%!!%">>]},
        m_payment:search_filter(#{<<"reference">> => <<"campaign_100%!">>})).

payment_filter_test() ->
    ?assertEqual(
        {<<" where payment_nr = $1">>, [<<"PAY-123">>]},
        m_payment:search_filter(#{<<"payment">> => <<"PAY-123">>})),
    ?assertEqual(
        {<<" where (id = $1 or payment_nr = $2)">>, [42, <<"42">>]},
        m_payment:search_filter(#{<<"payment">> => <<"42">>})).

combined_filter_test() ->
    Start = {{2026, 1, 1}, {0, 0, 0}},
    End = {{2027, 1, 1}, {0, 0, 0}},
    ?assertEqual(
        {<<" where (id = $1 or payment_nr = $2)"
           " and reference like $3 escape '!'"
           " and created >= $4 and created < $5">>,
         [42, <<"42">>, <<"donation-%">>, Start, End]},
        m_payment:search_filter(#{
            <<"payment">> => <<"42">>,
            <<"reference">> => <<"donation-">>,
            <<"year">> => <<"2026">>
        })).

%% Exercise the normalization used by both m.search and payment_export:data/1.
normalized_search_filter_test_() ->
    Cases = [
        {#{<<"reference">> => <<"donation-">>},
         {<<" where reference like $1 escape '!'">>, [<<"donation-%">>]}},
        {#{<<"reference">> => <<" campaign_100%! ">>},
         {<<" where reference like $1 escape '!'">>, [<<"campaign!_100!%!!%">>]}},
        {#{<<"payment">> => <<"PAY-123">>},
         {<<" where payment_nr = $1">>, [<<"PAY-123">>]}},
        {#{<<"payment">> => <<"42">>, <<"reference">> => <<"donation-">>,
           <<"year">> => <<"2026">>},
         {<<" where (id = $1 or payment_nr = $2)"
            " and reference like $3 escape '!'"
            " and created >= $4 and created < $5">>,
          [42, <<"42">>, <<"donation-%">>,
           {{2026, 1, 1}, {0, 0, 0}}, {{2027, 1, 1}, {0, 0, 0}}]}},
        {#{<<"payment">> => <<>>, <<"reference">> => <<>>, <<"year">> => undefined},
         {<<>>, []}}
    ],
    [?_assertEqual(Expected, m_payment:search_filter(z_search:props_to_map(Args)))
        || {Args, Expected} <- Cases].
