%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(directed_graph_algorithms).
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
            e -> 
                exit;
            _ ->
                 ok
        end end,
    HasPathFunctionFail = fun(Val) -> 
        case Val of 
            g -> 
                io:format("~nHas Path to: ~p ", [Val]),
                exit;
            _ ->
                ok
        end end,
    NodeMap = #{
        a => #graph_node{vertex = a, neighbours = [b, c]},
        b => #graph_node{vertex = b, neighbours = [d]},
        c => #graph_node{vertex = c, neighbours = [e]},
        d => #graph_node{vertex = d, neighbours = [f]},
        e => #graph_node{vertex = e},
        f => #graph_node{vertex = f},
        g => #graph_node{vertex = f}
    },
    StartNode = #graph_node{vertex = a, neighbours = [b, c]},

    io:format("Depth First~n"),
    DepthStoreGetter = fun pop_stack/1,
    DepthStorePutter = fun(NextNode, AccStack) -> [NextNode | AccStack] end,
    iterate_through_directed_graph_generic(PrintFunction, NodeMap, [StartNode], DepthStoreGetter, DepthStorePutter),
    HasPathToEDepth = iterate_through_directed_graph_generic(HasPathFunction, NodeMap, [StartNode], DepthStoreGetter, DepthStorePutter),
    HasPathToGDepth = iterate_through_directed_graph_generic(HasPathFunctionFail, NodeMap, [StartNode], DepthStoreGetter, DepthStorePutter),
    
    io:format("~nHas Path to E: ~p", [(HasPathToEDepth =:= exit)]),
    io:format("~nHas Path to G: ~p", [(HasPathToGDepth =:= exit)]),

    io:format("~nBreadth First~n"),
    BreadthStoreGetter = fun queue:out/1,
    BreadthStorePutter = fun queue:in/2,
    iterate_through_directed_graph_generic(PrintFunction, NodeMap, queue:from_list([StartNode]), BreadthStoreGetter, BreadthStorePutter),
    HasPathToEBreadth = iterate_through_directed_graph_generic(HasPathFunction, NodeMap, queue:from_list([StartNode]), BreadthStoreGetter, BreadthStorePutter),
    HasPathToGBreadth = iterate_through_directed_graph_generic(HasPathFunctionFail, NodeMap, queue:from_list([StartNode]), BreadthStoreGetter, BreadthStorePutter),
    
    io:format("~nHas Path to E: ~p", [(HasPathToEBreadth =:= exit)]),
    io:format("~nHas Path to G: ~p~n", [(HasPathToGBreadth =:= exit)]).

iterate_through_directed_graph_generic(_, _, [], _, _) ->
    ok;
iterate_through_directed_graph_generic(_, _, {[], []}, _, _) ->
    ok;
iterate_through_directed_graph_generic(Func, NodeMap, DataStore, DataStoreGetter, DataStorePutter) ->
    % Get next
    {{value, CurrentNode}, Rest} = DataStoreGetter(DataStore),

    % Apply function on node and see if we exit
    case Func(CurrentNode#graph_node.vertex) of
        exit -> exit;
        _ -> 
            UpdatedStore = lists:foldl(
                fun(NextNodeKey, Acc) ->
                    NextNode = maps:get(NextNodeKey, NodeMap),
                    DataStorePutter(NextNode, Acc)
                end,
                Rest,
                CurrentNode#graph_node.neighbours),

            % Loop again.
            iterate_through_directed_graph_generic(Func, NodeMap, UpdatedStore, DataStoreGetter, DataStorePutter)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pop_stack([]) ->
    {empty, []};
pop_stack([NextVal | RestOfStack]) ->
    {{value, NextVal}, RestOfStack}.