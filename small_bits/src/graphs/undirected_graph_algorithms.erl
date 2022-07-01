%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(undirected_graph_algorithms).
-author(nickw).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([
    test/0
]).

-record(graph_node, {
    vertex          :: any(),
    neighbours = [] :: list()
}).

%%%=============================================================================
%%% API
%%%=============================================================================

test() ->
    PrintFunction = fun(Val) -> io:format("Next Node: ~p ", [Val]) end,
    HasPathFunction = fun(Val) -> 
        case Val of 
            m -> 
                exit;
            _ ->
                 ok
        end end,
    HasPathFunctionFail = fun(Val) -> 
        case Val of 
            n -> 
                io:format("~nHas Path to: ~p ", [Val]),
                exit;
            _ ->
                ok
        end end,

    EdgesList = [
        [i, j],
        [k, i],
        [k, j],
        [m, k],
        [k, l],
        [o, n]
    ],

    NodeMap = convert_to_adjacency_map(EdgesList, #{}),

    Visited = sets:new(),

    StartNode = #graph_node{vertex = i, neighbours = [k,j]},

    io:format("Depth First~n"),
    DepthStoreGetter = fun pop_stack/1,
    DepthStorePutter = fun(NextNode, AccStack) -> [NextNode | AccStack] end,
    iterate_through_undirected_graph_generic(PrintFunction, NodeMap, [StartNode], DepthStoreGetter, DepthStorePutter, Visited),
    HasPathToEDepth = iterate_through_undirected_graph_generic(HasPathFunction, NodeMap, [StartNode], DepthStoreGetter, DepthStorePutter, Visited),
    HasPathToGDepth = iterate_through_undirected_graph_generic(HasPathFunctionFail, NodeMap, [StartNode], DepthStoreGetter, DepthStorePutter, Visited),
    
    io:format("~nHas Path to M: ~p", [(HasPathToEDepth =:= exit)]),
    io:format("~nHas Path to N: ~p", [(HasPathToGDepth =:= exit)]),

    io:format("~nBreadth First~n"),
    BreadthStoreGetter = fun queue:out/1,
    BreadthStorePutter = fun queue:in/2,
    iterate_through_undirected_graph_generic(PrintFunction, NodeMap, queue:from_list([StartNode]), BreadthStoreGetter, BreadthStorePutter, Visited),
    HasPathToEBreadth = iterate_through_undirected_graph_generic(HasPathFunction, NodeMap, queue:from_list([StartNode]), BreadthStoreGetter, BreadthStorePutter, Visited),
    HasPathToGBreadth = iterate_through_undirected_graph_generic(HasPathFunctionFail, NodeMap, queue:from_list([StartNode]), BreadthStoreGetter, BreadthStorePutter, Visited),
    
    io:format("~nHas Path to M: ~p", [(HasPathToEBreadth =:= exit)]),
    io:format("~nHas Path to N: ~p~n", [(HasPathToGBreadth =:= exit)]).

iterate_through_undirected_graph_generic(_, _, [], _, _, _) ->
    ok;
iterate_through_undirected_graph_generic(_, _, {[], []}, _, _, _) ->
    ok;
iterate_through_undirected_graph_generic(Func, NodeMap, DataStore, DataStoreGetter, DataStorePutter, Visited) ->
    % Get next
    {{value, CurrentNode}, Rest} = DataStoreGetter(DataStore),

    % Add to visited
    UpdatedVisited = sets:add_element(CurrentNode#graph_node.vertex, Visited),

    % Apply function on node and see if we exit
    case Func(CurrentNode#graph_node.vertex) of
        exit -> exit;
        _ -> 
            UpdatedStore = lists:foldl(
                fun(NextNodeKey, Acc) ->
                    % check if we have visited it
                    case sets:is_element(NextNodeKey, UpdatedVisited) of
                        false ->
                            NextNode = maps:get(NextNodeKey, NodeMap),
                            DataStorePutter(NextNode, Acc);
                        true ->
                            Acc
                    end
                end,
                Rest,
                CurrentNode#graph_node.neighbours),

            % Loop again.
        iterate_through_undirected_graph_generic(Func, NodeMap, UpdatedStore, DataStoreGetter, DataStorePutter, UpdatedVisited)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pop_stack([]) ->
    {empty, []};
pop_stack([NextVal | RestOfStack]) ->
    {{value, NextVal}, RestOfStack}.

convert_to_adjacency_map([], Map) ->
    Map;
convert_to_adjacency_map([[Fst, Snd] | Rst], Map) ->
    convert_to_adjacency_map(
        Rst,
        append_to_map_entry(Snd, Fst, append_to_map_entry(Fst, Snd, Map))
    ).

append_to_map_entry(Fst, Snd, Map) ->
    case maps:is_key(Fst, Map) of
        true -> maps:put(Fst, append_to_graph_node(maps:get(Fst, Map), Snd), Map);
        false -> maps:put(Fst, #graph_node{vertex = Fst, neighbours = [Snd]}, Map)
    end.

append_to_graph_node(GraphNode, NewNeighbour) ->
    GraphNode#graph_node{neighbours = [NewNeighbour | GraphNode#graph_node.neighbours]}.