%% @copyright 2026 Marc Worrell
%% @doc Scope context-based mocks so live sandbox workers retain real behavior.
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

-module(payment_test_mock).
-export([expect/4]).
-include_lib("zotonic_core/include/zotonic.hrl").

expect(Module, Function, Scope, Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    Dispatch = fun(Args) ->
        case test_context(Scope, lists:last(Args)) of
            true -> erlang:apply(Fun, Args);
            false -> meck:passthrough(Args)
        end
    end,
    Wrapper = case Arity of
        1 -> fun(A) -> Dispatch([A]) end;
        2 -> fun(A, B) -> Dispatch([A, B]) end;
        3 -> fun(A, B, C) -> Dispatch([A, B, C]) end;
        4 -> fun(A, B, C, D) -> Dispatch([A, B, C, D]) end;
        5 -> fun(A, B, C, D, E) -> Dispatch([A, B, C, D, E]) end
    end,
    meck:expect(Module, Function, Wrapper).

test_context(psp, undefined) -> true;
test_context(psp, #context{site = default}) -> true;
test_context(payment, #context{site = payment_receive_test}) -> true;
test_context(_, _) -> false.
