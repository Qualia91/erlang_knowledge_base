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

% cole_vishkin:start().

%% External API
-export([
    start_list/1
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_list(MaxVal) ->

    % Create ets table to store lined list information
    Tid = ets:new(linked_list_data, [ordered_set]),

    % Get length of maximum colour from max val in sequence
    LastBitString = linked_list_node:integer_to_bit_string(MaxVal),
    MaxBitStrLength = length(LastBitString),

    Self = self(),

    {ok, FirstPid} = linked_list_node:start_link(0, null, MaxBitStrLength, Self),
    ets:insert_new(Tid, {0, FirstPid, linked_list_node:pad_to_length(linked_list_node:integer_to_bit_string(0), MaxBitStrLength)}),

    % Start by making a linked list of processes
    LastPid = lists:foldl(
        fun(ItemIndex, PreviousPid) ->
            {ok, Pid} = linked_list_node:start_link(ItemIndex, PreviousPid, MaxBitStrLength, Self),
            ets:insert_new(Tid, {ItemIndex, Pid, linked_list_node:pad_to_length(linked_list_node:integer_to_bit_string(ItemIndex), MaxBitStrLength)}),
            Pid
        end,
        FirstPid,
        lists:seq(1, MaxVal)
    ),

    % Make the linked list cyclical as required by algorithm
    linked_list_node:set_previous_pid(FirstPid, LastPid),

    iterate(Tid, MaxVal).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
            linked_list_node:start_iteration(Pid)
        end,
        0,
        Tid
    ),

    % Now recieve all data back and sort to check
    _RetList = lists:sort(recieve_iter_data(MaxVal + 1, Tid)),
    
    % Update all nodes with new colours
    lager:info("Update node colours"),
    ets:foldl(
        fun({_, Pid, Colour}, _) ->
            linked_list_node:set_colour(Pid, Colour)
        end,
        0,
        Tid
    ),
    
    lager:info("Shared memory simulation object at end of iteration: ~p", [ets:tab2list(Tid)]).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
