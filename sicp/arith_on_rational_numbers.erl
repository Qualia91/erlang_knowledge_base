%%%-----------------------------------------------------------------------------
%%% @doc
%%% SICP page 113: Arithmetic operations for rational numbers.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(arith_on_rational_numbers).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    make_rat/2,
    numer/1,
    denom/1,
    add_rat/2,
    sub_rat/2,
    mul_rat/2,
    div_rat/2,
    equal_rat/2,
    print/1,
    gcd/2
]).

-record(rational_number, {
    numerator   :: integer(),
    denominator :: integer()
}).

%%%=============================================================================
%%% API
%%%=============================================================================

make_rat(NumIn, DenIn) when is_integer(NumIn), is_integer(DenIn) ->
    {Num, Den, Sign} = sign_fix(NumIn, DenIn),
    GCD = gcd(Num, Den),
    #rational_number{numerator = Sign * Num div GCD, denominator = Den div GCD};
make_rat(Num, Den) when is_float(Num), is_float(Den) ->
    make_rat(trunc(Num), trunc(Den)).

numer(#rational_number{numerator = Num}) ->
    Num.

denom(#rational_number{denominator = Den}) ->
    Den.

add_rat(X, Y) ->
    Num = (numer(X) * denom(Y)) + (numer(Y) * denom(X)),
    Den = denom(X) * denom(Y),
    make_rat(Num, Den).

sub_rat(X, Y) ->
    Num = (numer(X) * denom(Y)) - (numer(Y) * denom(X)),
    Den = denom(X) * denom(Y),
    make_rat(Num, Den).

mul_rat(X, Y) ->
    Num = numer(X) * numer(Y),
    Den = denom(X) * denom(Y),
    make_rat(Num, Den).

div_rat(X, Y) ->
    Num = numer(X) * denom(Y),
    Den = denom(X) * numer(Y),
    make_rat(Num, Den).

equal_rat(X, Y) ->
    numer(X) div denom(X) ==  numer(Y) div denom(Y).

print(#rational_number{numerator = Num, denominator = Den}) ->
    io:format("~p/~p~n", [Num, Den]).

gcd(X, Y) ->
    case Y of
        0 -> X;
        _ -> gcd(Y, X rem Y)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

sign_fix(Num, Den) when Num >= 0 ->
    {abs(Num), abs(Den), 1};
sign_fix(Num, Den) ->
    {abs(Num), abs(Den), -1}.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ration_arith_test() ->
    OneThird = arith_on_rational_numbers:make_rat(1, 3),
    TwoThirds = arith_on_rational_numbers:add_rat(OneThird, OneThird),
    ?assertEqual(2, arith_on_rational_numbers:num(TwoThirds)),
    ?assertEqual(3, arith_on_rational_numbers:num(TwoThirds)).

-endif.