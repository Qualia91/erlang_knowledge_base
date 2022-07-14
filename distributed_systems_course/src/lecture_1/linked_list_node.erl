%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(linked_list_node).
-author(boc_dev).
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/4,
    start_iteration/1,
    get_colour/2,
    set_colour/2,
    set_previous_pid/2,
    send_next_node_colour/2,
    integer_to_bit_string/1,
    pad_to_length/2
]).

%% Callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

%% Internal functions for tests
-export([
    cole_vishkin_colour_reduction/2,
    compare_and_return/3,
    convert_pair_to_binary_string/1
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
    index             :: integer(),
    previous_item_pid :: pid(),
    colour            :: string(),
    database_pid      :: pid()
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(integer(), pid(), integer(), pid()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Index, PreviousItemPid, BitStringLength, DatabasePid) ->
    gen_server:start_link(?MODULE, [Index, PreviousItemPid, BitStringLength, DatabasePid], []).

start_iteration(Pid) ->
    gen_server:cast(Pid, iterate).

get_colour(Pid, ReturnAddress) ->
    gen_server:cast(Pid, {get_colour, ReturnAddress}).

set_colour(Pid, Colour) ->
    gen_server:cast(Pid, {set_colour, Colour}).

set_previous_pid(Pid, PreviousPid) ->
    gen_server:cast(Pid, {set_previous_pid, PreviousPid}).

send_next_node_colour(Pid, Colour) ->
    gen_server:cast(Pid, {prev_node_colour, Colour}).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([Index, PreviousItemPid, BitStringLength, DatabasePid]) ->
    StartingColour = pad_to_length(integer_to_bit_string(Index), BitStringLength),
    LoopState = #loop_state{
        index             = Index,
        previous_item_pid = PreviousItemPid,
        colour            = StartingColour,
        database_pid      = DatabasePid
    },
    lager:debug("Starting node ~p with colour ~p", [Index, StartingColour]),
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(Request, _From, LoopState) ->
    Reply = ok,
    lager:debug("Other handle_call: ~p~n", [Request]),
    {reply, Reply, LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast({set_colour, NewColour}, LoopState) ->
    {noreply, LoopState#loop_state{colour = NewColour}};
handle_cast({get_colour, ReturnAddress}, LoopState = #loop_state{colour = Colour}) ->
    linked_list_node:send_next_node_colour(ReturnAddress, Colour),
    {noreply, LoopState};
handle_cast(iterate, LoopState = #loop_state{previous_item_pid = PrevItemPid}) ->
    linked_list_node:get_colour(PrevItemPid, self()),
    {noreply, LoopState};
handle_cast({prev_node_colour, PreviousColour}, LoopState = #loop_state{index = Index, colour = Colour, database_pid = DatabasePid}) ->
    NewColour = cole_vishkin_colour_reduction(Colour, PreviousColour),
    DatabasePid ! {update_colour, Index, NewColour},
    {noreply, LoopState};
handle_cast({set_previous_pid, Pid}, LoopState) ->
    {noreply, LoopState#loop_state{previous_item_pid = Pid}};
handle_cast(Msg, LoopState) ->
    lager:debug("Other handle_cast: ~p~n", [Msg]),
    {noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(Info, LoopState) ->
    lager:debug("Other handle_info: ~p~n", [Info]),
    {noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
    lager:debug("Terminating~n"),
    ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

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

% This function, as the algorithm requires, assumes that each starting colour is unque, and will fail if not.
cole_vishkin_colour_reduction(CurrentItemColour, PreviousItemColour) ->
    convert_pair_to_binary_string(
        lists:foldl(
            fun({CurentColChar, PrevColChar}, Acc) ->
                compare_and_return(CurentColChar, PrevColChar, Acc)
            end,
            not_found,
            lists:zip(CurrentItemColour, PreviousItemColour)
        )
    ).

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

%%%===================================================================
%%% Tests
%%%===================================================================

% -ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

integer_to_bit_string_test() ->

    TestCases = [
        {5,     "101"             },
        {9,     "1001"            },
        {11,    "1011"            },
        {37862, "1001001111100110"}
    ],

    action_test_cases(TestCases, fun(Input) -> integer_to_bit_string(Input) end).

pad_to_length_test() ->

    TestCases = [
        {{"101",              16}, "0000000000000101"},
        {{"1001",             16}, "0000000000001001"},
        {{"1011",             16}, "0000000000001011"},
        {{"1001001111100110", 16}, "1001001111100110"}
    ],

    action_test_cases(TestCases, fun({Input, PaddingLength}) -> pad_to_length(Input, PaddingLength) end).

cole_vishkin_colour_reduction_test() ->

    TestCases = [
        {{"0110010000", "1010010000"}, "10001"},
        {{"1010010000", "0010110000"}, "1010" },
        {{"10001"     , "01010"     }, "1"    }
    ],

    action_test_cases(TestCases, fun({StartItem, PreviousItemColour}) -> cole_vishkin_colour_reduction(StartItem, PreviousItemColour) end).

convert_pair_to_binary_string_test() ->

    TestCases = [
        {{8, $1}, "10001"},
        {{5, $0}, "1010" },
        {{0, $1}, "1" }
    ],

    action_test_cases(TestCases, fun(Input) -> convert_pair_to_binary_string(Input) end).

%%%===================================================================
%%% Internal Test Functions
%%%===================================================================

action_test_cases(TestCases, Function) ->
    lists:foreach(
        fun({Input, ExpectedOutput}) ->
            ?assertEqual(ExpectedOutput, Function(Input))
        end,
        TestCases
    ).

% -endif.
