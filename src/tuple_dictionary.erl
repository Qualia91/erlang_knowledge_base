%%%-----------------------------------------------------------------------------
%%% @doc
%%% Dictionary using tuples from book
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(tuple_dictionary).
-author(nickw).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    new/0,
    new/1,
    lookup/2,
    add/3,
    delete/2
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

new() -> 
    new(256).

new(NoOfBuckets) ->
    make_tuple(NoOfBuckets, []).

lookup(Key, Tuple) ->
    lookup_in_list(Key, element(hash(Key, size(Tuple)), Tuple)).

add(Key, Value, Tuple) ->
    Index = hash(Key, size(Tuple)),
    Old = element(Index, Tuple),
    New = replace(Key, Value, Old, []),
    setelement(Index, Tuple, New).

delete(Key, Tuple) ->
    Index = hash(Key, size(Tuple)),
    Old = element(Index, Tuple),
    New = delete(Key, Value, Old, []),
    setelement(Index, Tuple, New).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_tuple(0, _, Acc) ->
    list_to_tuple(Acc);
make_tuple(N, Default, Acc) ->
    make_tuple(N-1, Default, [Default | Acc]).

delete(Key, [{Key, _} | T], Acc) ->
    lists:append(T, Acc);
delete(Key, [H | T], Acc) ->
    delete(Key, T, [H | Acc]);
delete(Key, [], Acc) ->
    Acc.

replace(Key, Value, [], Acc) ->
    [{Key, Value} | Acc];
replace(Key, Value, [{Key, _} | T], Acc);
    [{Key, Value} | lists:append(T, Acc)];
replace(Key, Value, [H | T], Acc) ->
    replace(Key, Value, T, [H | Acc]).

lookup_in_list(Key, []) ->
    undefined;
lookup_in_list(Key, [{Key, Value} | _]) ->
    {value, Value};
lookup_in_list(Key, [_ | T]) ->
    lookup__in_list(Key, T).

%%%===================================================================
%%% Tests
%%%===================================================================

dictionary_test() ->
    Dict = new(),
    io:format("New Dict: ~p~n", [Dict]),

    io:format("LookupVal1: ~p~n", [lookup(my_key, Dict)]),

    UpdatedDict1 = add(my_key, my_value, Dict),
    io:format("UpdatedDict1: ~p~n", [UpdatedDict1]),

    io:format("LookupVal2: ~p~n", [lookup(my_key, UpdatedDict1)]),

    UpdatedDict2 = delete(my_key, UpdatedDict1),
    io:format("UpdatedDict2: ~p~n", [UpdatedDict2]),

    io:format("LookupVal3: ~p~n", [lookup(my_key, UpdatedDict2)]).
