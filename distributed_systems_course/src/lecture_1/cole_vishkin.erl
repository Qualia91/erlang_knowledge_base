%%%-----------------------------------------------------------------------------
%%% @doc
%%% Example of Cole-Vishkin algorithm to colour linked lists in O(log*n) + O{1).
%%% From theory of distriibuted systems course, lecture 1.
%%%
%%% Explanation:
%%% The link list is a group of processes that hold a pid to the previous node in the list.
%%% 
%%% Note:
%%% This is a demonstration of the process involved, and not actually correct due to the 
%%% fact you don't get shared memoory in erlang.
%%% Also, the sending back to the ets table is just for debugging ease. The data structure 
%%% is the processes we made along the way...
%%% Think this only works for simple graphs (undirected, loop free, unweighted and without parallel edges), so _1 and _2 don't work
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(cole_vishkin).
-author(boc_dev).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

% cole_vishkin:start_list(3).
% cole_vishkin:start_tree_graph(0).

%% External API
-export([
    start_list/1,
    start_tree_graph/0,
    start_tree_graph_2/0,
    start_tree_graph_3/0,
    integer_to_bit_string/1,
    pad_to_length/2,
    cole_vishkin_colour_reduction/2
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_list(MaxVal) ->

    % Create ets table to store lined list information
    Tid = ets:new(linked_list_data, [ordered_set]),

    % Get length of maximum colour from max val in sequence
    LastBitString = integer_to_bit_string(MaxVal),
    MaxBitStrLength = length(LastBitString),

    Self = self(),

    {ok, FirstPid} = distributed_node:start_link(0, MaxBitStrLength, Self),
    ets:insert_new(Tid, {0, FirstPid, pad_to_length(integer_to_bit_string(0), MaxBitStrLength)}),

    % Start by making a linked list of processes
    LastPid = lists:foldl(
        fun(ItemIndex, PreviousPid) ->
            {ok, Pid} = distributed_node:start_link(ItemIndex, MaxBitStrLength, Self),
            distributed_node:set_neighbours(Pid, [PreviousPid]),
            Colour = pad_to_length(integer_to_bit_string(ItemIndex), MaxBitStrLength),
            ets:insert_new(Tid, {ItemIndex, Pid, Colour}),
            Pid
        end,
        FirstPid,
        lists:seq(1, MaxVal)
    ),

    distributed_node:set_neighbours(FirstPid, [LastPid]),

    iterate(Tid, MaxVal).

start_tree_graph() ->

% Input
%     0
%    / \
%   1   2
%  / \   \
% 3   4   5

    % Create ets table to store lined list information
    Tid = ets:new(linked_list_data, [ordered_set]),

    % Get length of maximum colour from max val in sequence
    LastBitString = integer_to_bit_string(5),
    MaxBitStrLength = length(LastBitString),

    Pid0 = create_node(0, Tid, MaxBitStrLength),
    Pid1 = create_node(1, Tid, MaxBitStrLength),
    Pid2 = create_node(2, Tid, MaxBitStrLength),
    Pid3 = create_node(3, Tid, MaxBitStrLength),
    Pid4 = create_node(4, Tid, MaxBitStrLength),
    Pid5 = create_node(5, Tid, MaxBitStrLength),
    
    distributed_node:set_neighbours(Pid0, [Pid3, Pid4, Pid5]),
    distributed_node:set_neighbours(Pid1, [Pid0]),
    distributed_node:set_neighbours(Pid2, [Pid0]),
    distributed_node:set_neighbours(Pid3, [Pid1]),
    distributed_node:set_neighbours(Pid4, [Pid1]),
    distributed_node:set_neighbours(Pid5, [Pid2]),

    iterate(Tid, 5).

start_tree_graph_2() ->

% Input
%     0
%    / \
%   1   2
%  / \ / \
% 3   4   5

    % Create ets table to store lined list information
    Tid = ets:new(linked_list_data, [ordered_set]),

    % Get length of maximum colour from max val in sequence
    LastBitString = integer_to_bit_string(5),
    MaxBitStrLength = length(LastBitString),

    Pid0 = create_node(0, Tid, MaxBitStrLength),
    Pid1 = create_node(1, Tid, MaxBitStrLength),
    Pid2 = create_node(2, Tid, MaxBitStrLength),
    Pid3 = create_node(3, Tid, MaxBitStrLength),
    Pid4 = create_node(4, Tid, MaxBitStrLength),
    Pid5 = create_node(5, Tid, MaxBitStrLength),
    
    distributed_node:set_neighbours(Pid0, [Pid3, Pid4, Pid5]),
    distributed_node:set_neighbours(Pid1, [Pid0]),
    distributed_node:set_neighbours(Pid2, [Pid0]),
    distributed_node:set_neighbours(Pid3, [Pid1]),
    distributed_node:set_neighbours(Pid4, [Pid1, Pid2]),
    distributed_node:set_neighbours(Pid5, [Pid2]),

    iterate(Tid, 5).

start_tree_graph_3() ->

% Input
%     0
%    / \
%   1   2
%  / \ / \
% |   3   4
%  \   \ /
%   5   6
%    \ /
%     7
    % Create ets table to store lined list information
    Tid = ets:new(linked_list_data, [ordered_set]),

    % Get length of maximum colour from max val in sequence
    LastBitString = integer_to_bit_string(7),
    MaxBitStrLength = length(LastBitString),

    Pid0 = create_node(0, Tid, MaxBitStrLength),
    Pid1 = create_node(1, Tid, MaxBitStrLength),
    Pid2 = create_node(2, Tid, MaxBitStrLength),
    Pid3 = create_node(3, Tid, MaxBitStrLength),
    Pid4 = create_node(4, Tid, MaxBitStrLength),
    Pid5 = create_node(5, Tid, MaxBitStrLength),
    Pid6 = create_node(6, Tid, MaxBitStrLength),
    Pid7 = create_node(7, Tid, MaxBitStrLength),
    
    distributed_node:set_neighbours(Pid0, [Pid7]),
    distributed_node:set_neighbours(Pid1, [Pid0]),
    distributed_node:set_neighbours(Pid2, [Pid0]),
    distributed_node:set_neighbours(Pid3, [Pid1, Pid2]),
    distributed_node:set_neighbours(Pid4, [Pid2]),
    distributed_node:set_neighbours(Pid5, [Pid1]),
    distributed_node:set_neighbours(Pid6, [Pid3, Pid4]),
    distributed_node:set_neighbours(Pid7, [Pid5, Pid6]),

    iterate(Tid, 7).

integer_to_bit_string(Int) ->
    integer_to_bit_string(Int, "").
    
pad_to_length(BitString, ExpectedLength) when length(BitString) < ExpectedLength ->
    % Make a list of 0's of how short it is, and concatenate onto existing list
    lists:concat([[$0 || _ <- lists:seq(1, ExpectedLength - length(BitString))], BitString]);
pad_to_length(BitString, _ExpectedLength) ->
    BitString.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_node(Index, Tid, MaxBitStrLength) ->
    {ok, Pid} = distributed_node:start_link(Index, MaxBitStrLength, self()),
    ets:insert_new(Tid, {Index, Pid, pad_to_length(integer_to_bit_string(Index), MaxBitStrLength)}),
    Pid.

recieve_iter_data(0, _Tid) ->
    [];
recieve_iter_data(NumRemaining, Tid) ->
    Resp = 
        receive
            {update_colour, Index, NewColour} ->
                [{Index, Pid, _Color}] = ets:lookup(Tid, Index),
                lager:debug("NewColour: ~p", [NewColour]),
                ets:insert(Tid, {Index, Pid, NewColour}),
                {Index, NewColour};
            Other ->
                lager:debug("Other Message: ~p", [Other])
        end,
    [Resp | recieve_iter_data(NumRemaining - 1, Tid)].

iterate(Tid, MaxVal) ->
    lager:info("Shared memory simulation object at start of iteration: ~p", [ets:tab2list(Tid)]),

    % Now apply Cole-Vishkin colour reduction algorithm by sending start message to all node processes
    lager:info("Starting iteration"),
    ets:foldl(
        fun({_, Pid, _}, _) ->
            distributed_node:start_iteration(Pid)
        end,
        0,
        Tid
    ),

    % Now recieve all data back and sort to check
    _RetList = lists:sort(recieve_iter_data(MaxVal + 1, Tid)),
    
    lager:info("Shared memory simulation object at end of iteration: ~p", [ets:tab2list(Tid)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

integer_to_bit_string(0, String) ->
    String;
integer_to_bit_string(Int, String) ->
    NextChar = case Int rem 2 of
        0 ->
            $0;
        1 ->
            $1
    end,
    integer_to_bit_string(Int div 2, [NextChar | String]).

% This function, as the algorithm requires, assumes that the started colours are correct, and will fail if not.
cole_vishkin_colour_reduction(CurrentColour, []) ->
    CurrentColour;
cole_vishkin_colour_reduction(CurrentColour, [NeighbourColour | Tl]) ->
    UpdatedColour = pad_to_length(
        convert_pair_to_binary_string(
            lists:foldl(
                fun({CurentColChar, PrevColChar}, Acc) ->
                    compare_and_return(CurentColChar, PrevColChar, Acc)
                end,
                not_found,
                lists:zip(CurrentColour, NeighbourColour)
            )
        ),
        length(NeighbourColour)
    ),
    cole_vishkin_colour_reduction(UpdatedColour, Tl).

% The point of this function is to find the character and index of the current string we are searching for
% that doesnt match the previous string we are comparing against, yet as if we were reading it right to left.
% It ios done that way to cut out the need for a lists:reverse.
compare_and_return(CurentColChar, PrevColChar, _) when CurentColChar =/= PrevColChar ->
    {0, CurentColChar};
compare_and_return(_CurentColChar, _PrevColChar, not_found) ->
    not_found;
compare_and_return(_CurentColChar, _PrevColChar, {Counter, TargetCharacter}) ->
    {Counter + 1, TargetCharacter}.

convert_pair_to_binary_string({Index, TargetCharacter}) ->
    integer_to_bit_string(Index) ++ [TargetCharacter].

strip_leading_zeros([]) ->
    "0";
strip_leading_zeros([$0 | Tl]) ->
    strip_leading_zeros(Tl);
strip_leading_zeros([$1 | Tl]) ->
    [$1 | Tl].

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

integer_to_bit_string_test() ->

    TestCases = [
        {5,     "101"             },
        {9,     "1001"            },
        {11,    "1011"            },
        {37862, "1001001111100110"}
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> integer_to_bit_string(Input) end).

pad_to_length_test() ->

    TestCases = [
        {{"101",              16}, "0000000000000101"},
        {{"1001",             16}, "0000000000001001"},
        {{"1011",             16}, "0000000000001011"},
        {{"1001001111100110", 16}, "1001001111100110"}
    ],

    test_utilities:action_test_cases(TestCases, fun({Input, PaddingLength}) -> pad_to_length(Input, PaddingLength) end).

cole_vishkin_colour_reduction_test() ->

    TestCases = [
        {{"0110010000", ["1010010000", "0010110000"]}, "0000000001"},
        {{"0000000000", ["0000000001", "0000000010", "0000000100"]}, "0000000011"}
    ],

    test_utilities:action_test_cases(TestCases, fun({StartColour, NeighbourColours}) -> cole_vishkin_colour_reduction(StartColour, NeighbourColours) end).

convert_pair_to_binary_string_test() ->

    TestCases = [
        {{8, $1}, "10001"},
        {{5, $0}, "1010" },
        {{0, $1}, "1"    }
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> convert_pair_to_binary_string(Input) end).

strip_leading_zeros_test() ->

    TestCases = [
        {"10001", "10001"},
        {"00001", "1"    },
        {"01010", "1010" },
        {"0",     "0"    }
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> strip_leading_zeros(Input) end).

%%%===================================================================
%%% Internal Test Functions
%%%===================================================================

-endif.

