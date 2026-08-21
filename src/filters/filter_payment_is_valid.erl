%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test if an amount and currency can be used for a payment request.
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

-module(filter_payment_is_valid).
-moduledoc("
Test if an amount and currency can be used for a payment request.

Use as: amount|payment_is_valid:currency
").

-export([payment_is_valid/3]).

-spec payment_is_valid(term(), term(), z:context()) -> boolean().
payment_is_valid(Amount, Currency, _Context) ->
    mod_payment:is_valid_payment_args(Amount, Currency).
