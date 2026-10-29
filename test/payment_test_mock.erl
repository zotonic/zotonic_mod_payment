%% @doc Scope context-based mocks so live sandbox workers retain real behavior.
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
