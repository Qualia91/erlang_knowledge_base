%%%-----------------------------------------------------------------------------
%%% @doc
%%% SICP page 198: SImple symbolic differentiation.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(symbolic_differentiation).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    deriv/2
]).

%%%=============================================================================
%%% API
%%%=============================================================================

deriv(Exp, Var) ->
    conditional([
        {is_number(Exp), fun deriv_if_number/2},
        {is_variable(Exp), fun deriv_if_variable/2},
        {is_sum(Exp), fun deriv_if_sum/2},
        {is_product(Exp), fun deriv_if_prod/2},
        {else, fun deriv_if_error/2}
    ], Exp, Var).

is_variable(Var) when is_atom(Var) ->
    true;
is_variable(_) ->
    false.

is_same_variable(Var1, Var2) when is_atom(Var1), is_atom(Var2) ->
    Var1 =:= Var2;
is_same_variable(_, _) ->
    false.

is_sum(['+' | _]) ->
    true;
is_sum(_) ->
    false.

addend(['+', Var, _]) ->
    Var.

augend(['+', _, Var]) ->
    Var.

make_sum(Var1, Var2) ->
    conditional([
        {equals_number(Var1, 0), fun(_, _) -> Var2 end},
        {equals_number(Var2, 0), fun(_, _) -> Var1 end},
        {is_number(Var1) and is_number(Var2), fun(_, _) -> Var1 + Var2 end},
        {else, fun(_, _) -> ['+', Var1, Var2] end}
    ], Var1, Var2).
    % ['+', Var1, Var2].

is_product(['*' | _]) ->
    true;
is_product(_) ->
    false.

multiplier(['*', Var, _]) ->
    Var.

multiplicand(['*', _, Var]) ->
    Var.

make_product(Var1, Var2) ->
    conditional([
        {equals_number(Var1, 0) or equals_number(Var2, 0), fun(_, _) -> 0 end},
        {equals_number(Var1, 1), fun(_, _) -> Var2 end},
        {equals_number(Var2, 1), fun(_, _) -> Var1 end},
        {is_number(Var1) and is_number(Var2), fun(_, _) -> Var1 * Var2 end},
        {else, fun(_, _) -> ['*', Var1, Var2] end}
    ], Var1, Var2).

conditional([], _, _) ->
    ok;
conditional([{Condition, Fun} | Rest], Exp, Var) ->
    case Condition of
        else  -> Fun(Exp, Var);
        true  -> Fun(Exp, Var);
        false -> conditional(Rest, Exp, Var)
    end.

equals_number(Exp, Var) when is_number(Exp), is_number(Var) -> 
    Exp == Var;
equals_number(_, _) ->
    false.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

deriv_if_number(_, _) ->
    0.

deriv_if_variable(Exp, Var) ->
    case is_same_variable(Exp, Var) of
        true  -> 1;
        false -> 0
    end.

deriv_if_sum(Exp, Var) ->
    make_sum(
        deriv(addend(Exp), Var),
        deriv(augend(Exp), Var)
    ).

deriv_if_prod(Exp, Var) ->
    make_sum(
        make_product(multiplier(Exp), deriv(multiplicand(Exp), Var)),
        make_product(deriv(multiplier(Exp), Var), multiplicand(Exp))
    ).

deriv_if_error(Exp, _) ->
    io:format("Error, unknown expressions: ~p~n", [Exp]).

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

deriv_test() ->
    ?assertEqual(1, symbolic_differentiation:deriv(['+', x, 3], x)),
    ?assertEqual(['*',1,y], symbolic_differentiation:deriv(['*', x, y], x)),
    ?assertEqual(['+',['*',x,y],['*',y,['*',x,3]]], symbolic_differentiation:deriv(['*', ['*',x,y], ['+',x,3]], x)).

-endif.