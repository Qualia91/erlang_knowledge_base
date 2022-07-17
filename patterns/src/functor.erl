%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(functor).
-author(boc_dev).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% Without functor pattern
-export([
    maybe_int_to_string/1,
    either_int_to_string/1,
    list_int_to_string/1
]).

%% With functor pattern
-export([
    map/2
]).

-define(SERVER, ?MODULE).

-type maybe(Type)   :: Type | nothing.
-type either(Type)  :: {left, Type} | {right, Type}.
-type functor(Type) :: maybe(any()) | either(Type) | list(Type) | Type.
-type map_fn(Type)  :: fun((Type) -> Type).

%%%=============================================================================
%%% Without functor pattern
%%%=============================================================================

maybe_int_to_string(nothing) ->
    nothing;
maybe_int_to_string(Val) when is_integer(Val) ->
    integer_to_list(Val).

either_int_to_string({right, Val}) when is_integer(Val) ->
    {right, integer_to_list(Val)};
either_int_to_string({left, Val}) when is_integer(Val) ->
    {left, Val}.

list_int_to_string([Val | Tl]) when is_integer(Val) ->
    [integer_to_list(Val) | list_int_to_string(Tl)];
list_int_to_string([]) ->
    [].

%%%=============================================================================
%%% With functor pattern
%%%=============================================================================

-spec map(map_fn(Type), functor(Type)) -> functor(Type) when Type :: any().
map(_MapFunc, nothing) ->
    nothing;
map(MapFunc, {right, Val}) ->
    {right, MapFunc(Val)};
map(_MapFunc, {left, Val}) ->
    {left, Val};
map(MapFunc, [Val | Tl]) ->
    [MapFunc(Val) | map(MapFunc, Tl)];
map(_MapFunc, []) ->
    [];
map(MapFunc, Val) ->
    MapFunc(Val).

-spec map_int_to_string(functor(integer())) -> functor(integer()).
map_int_to_string(Val) ->
    map(fun integer_to_list/1, Val).

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_without_functor_test() ->
    Cases = [
        {nothing, nothing, fun maybe_int_to_string/1},
        {1, "1", fun maybe_int_to_string/1},
        {{left, 1}, {left, 1}, fun either_int_to_string/1},
        {{right, 1}, {right, "1"}, fun either_int_to_string/1},
        {[1,2,3], ["1", "2", "3"], fun list_int_to_string/1}
    ],
    
    run_assert(Cases).

example_with_functor_test() ->
    Cases = [
        {nothing, nothing, fun map_int_to_string/1},
        {1, "1", fun map_int_to_string/1},
        {{left, 1}, {left, 1}, fun map_int_to_string/1},
        {{right, 1}, {right, "1"}, fun map_int_to_string/1},
        {[1,2,3], ["1", "2", "3"], fun map_int_to_string/1}
    ],
    
    run_assert(Cases).

%%%=============================================================================
%%% Internal test functions
%%%=============================================================================

run_assert([]) ->
    ok;
run_assert([{Input, Expected, Function} | Tl]) ->
    ?assertEqual(Expected, Function(Input)),
    run_assert(Tl).

-endif.
