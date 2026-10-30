%% @copyright 2026 Marc Worrell
%% @doc Validation of normalized, authenticated PSP payment updates.
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

-module(payment_receive).
-export([validate/1, matches/3]).

-spec validate(Data) -> Result
    when Data :: map(), Result :: ok | {error, payment_data}.
validate(#{psp_external_id := Id, status := Status, status_date := Date} = Data) ->
    Valid = is_binary(Id) andalso byte_size(Id) > 0 andalso byte_size(Id) =< 128
        andalso lists:member(Status, [new, pending, paid, cancelled, failed, refunded, error])
        andalso valid_date(Date)
        andalso valid_amount(maps:get(amount, Data, undefined))
        andalso valid_currency(maps:get(currency, Data, undefined))
        andalso valid_event_id(maps:get(event_id, Data, undefined))
        andalso valid_reference(maps:get(payment_nr, Data, undefined))
        andalso is_map(maps:get(contact, Data, #{})),
    case Valid of
        true -> ok;
        false -> {error, payment_data}
    end;
validate(_) ->
    {error, payment_data}.

%% @doc A supplied local reference may never reassign a different PSP payment.
%% Missing PSP identity means initialization is still in progress: retry later.
-spec matches(PspModule, Data, Payment) -> boolean()
    when PspModule :: module(), Data :: map(), Payment :: map().
matches(PspModule, #{psp_external_id := ExtId} = Data, Payment) ->
    maps:get(<<"psp_module">>, Payment, undefined) =:= PspModule
        andalso maps:get(<<"psp_external_id">>, Payment, undefined) =:= ExtId
        andalso matches_currency(maps:get(currency, Data, undefined), Payment)
        andalso matches_amount(maps:get(amount, Data, undefined), Payment).

matches_currency(undefined, _) -> true;
matches_currency(Currency, Payment) -> Currency =:= maps:get(<<"currency">>, Payment).
matches_amount(undefined, _) -> true;
matches_amount(Amount, Payment) -> abs(Amount - maps:get(<<"amount">>, Payment)) < 0.0000001.

valid_amount(undefined) -> true;
valid_amount(Amount) -> is_number(Amount) andalso Amount >= 0.
valid_currency(undefined) -> true;
valid_currency(<<A, B, C>>) ->
    A >= $A andalso A =< $Z andalso B >= $A andalso B =< $Z andalso C >= $A andalso C =< $Z;
valid_currency(_) -> false.
valid_event_id(undefined) -> true;
valid_event_id(Id) -> is_binary(Id) andalso byte_size(Id) =< 128.
valid_reference(undefined) -> true;
valid_reference(null) -> true;
valid_reference(Ref) -> is_binary(Ref) andalso byte_size(Ref) =< 256.
valid_date({{Y, M, D}, {H, I, S}})
    when is_integer(Y), Y > 0, is_integer(M), is_integer(D),
         is_integer(H), H >= 0, H < 24, is_integer(I), I >= 0, I < 60,
         is_integer(S), S >= 0, S < 60 ->
    calendar:valid_date({Y, M, D});
valid_date(_) -> false.
