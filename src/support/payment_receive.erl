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
        andalso is_boolean(maps:get(is_existing_only, Data, false))
        andalso is_boolean(maps:get(is_status_snapshot, Data, false))
        andalso valid_discount(Data)
        andalso valid_currency(maps:get(currency, Data, undefined))
        andalso valid_event_id(maps:get(event_id, Data, undefined))
        andalso valid_profile(maps:get(psp_profile_id, Data, undefined))
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
        andalso matches_amount(Data, Payment).

matches_currency(undefined, _) -> true;
matches_currency(Currency, Payment) -> Currency =:= maps:get(<<"currency">>, Payment).
%% Discount fields are supplied only by authenticated PSP adapters. The original
%% amount is immutable; a discount never permits a different order value.
matches_amount(#{original_amount := Original} = Data, Payment) ->
    Props = maps:get(<<"props">>, Payment, #{}),
    Expected = case Props of
        #{<<"original_amount">> := Saved} -> Saved;
        _ -> original_amount(Payment)
    end,
    valid_discount(Data) andalso equal_amount(Original, Expected);
matches_amount(Data, Payment) ->
    case maps:get(amount, Data, undefined) of
        undefined -> true;
        Amount -> equal_amount(Amount, maps:get(<<"amount">>, Payment))
    end.

%% Legacy imports stored the net amount. Recover their gross baseline only from
%% the stored authenticated PSP payload, never from the incoming update alone.
%% Local checkout prices remain authoritative. Recurring child records were also
%% imported at their provider's net amount before discount reconciliation existed.
original_amount(#{<<"psp_module">> := Module} = Payment) ->
    IsImport = maps:get(<<"is_external">>, Payment, false)
        orelse is_integer(maps:get(<<"recurring_payment_id">>, Payment, undefined)),
    case IsImport of
        true -> recovered_original(Module, Payment);
        false -> maps:get(<<"amount">>, Payment)
    end.

recovered_original(Module, Payment) ->
    _ = code:ensure_loaded(Module),
    try Module:normalize_payment(maps:get(<<"psp_data">>, Payment, #{})) of
        {ok, #{original_amount := Original, amount := Net,
               currency := Currency, psp_external_id := Id} = Saved} ->
            case validate(Saved) =:= ok
                 andalso equal_amount(Net, maps:get(<<"amount">>, Payment))
                 andalso Currency =:= maps:get(<<"currency">>, Payment)
                 andalso Id =:= maps:get(<<"psp_external_id">>, Payment) of
                true -> Original;
                false -> undefined
            end;
        _ -> undefined
    catch
        _:_ -> undefined
    end.

valid_discount(#{original_amount := Original, discount_amount := Discount,
                 amount := Amount} = Data) ->
    is_number(Original) andalso is_number(Discount) andalso is_number(Amount)
        andalso Original >= 0 andalso Discount >= 0 andalso Amount >= 0
        andalso equal_amount(Original, Amount + Discount)
        andalso is_map(maps:get(discount_details, Data, #{}));
valid_discount(Data) ->
    not maps:is_key(original_amount, Data)
        andalso not maps:is_key(discount_amount, Data)
        andalso not maps:is_key(discount_details, Data).

equal_amount(A, B) when is_number(A), is_number(B) -> abs(A - B) < 0.0000001;
equal_amount(_, _) -> false.

valid_profile(undefined) -> true;
valid_profile(P) -> is_binary(P) andalso byte_size(P) > 0 andalso byte_size(P) =< 128.

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
