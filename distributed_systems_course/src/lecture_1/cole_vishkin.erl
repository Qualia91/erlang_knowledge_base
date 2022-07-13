%%%-----------------------------------------------------------------------------
%%% @doc
%%% Example of Cole-Vishkin algorithm to colour linked lists in O(log*n) + O{1).
%%% From theory of distriibuted systems course, lecture 1.
%%%
%%% Explanation:
%%% 
%%% 
%%% 
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

%% External API
-export([
    start/0
]).

%% Internal functions for tests
-export([
    integer_to_bit_string/1,
    pad_to_length/2,
    cole_vishkin_colour_reduction/2,
    compare_and_return/3,
    convert_pair_to_binary_string/1
]).

-define(SERVER, ?MODULE).

-record(item, {
    unique_index          :: integer(),
    previous_unique_index :: integer(),
    colour                :: string()
}).

%%%=============================================================================
%%% API
%%%=============================================================================

start() ->
    % Create ets table to store lined list information
    Tid = ets:new(linked_list_data, []),

    % Get length of maximum colour from max val in sequence
    MaxVal = 3,
    LastBitString = integer_to_bit_string(MaxVal),
    MaxBitStrLength = length(LastBitString),
    % Start by making a large linked list of objects
    _StartingList = lists:foldl(
        fun(ItemIndex, PreviousItemIndex) ->
            Item = #item{
                unique_index          = ItemIndex,
                previous_unique_index = PreviousItemIndex,
                colour                = pad_to_length(integer_to_bit_string(ItemIndex), MaxBitStrLength)
            },
            ets:insert_new(Tid, {ItemIndex, Item}),
            ItemIndex
        end,
    MaxVal,
    lists:seq(0, MaxVal)),

    ets:tab2list(Tid).

    % Now apply Cole-Vishkin colour reduction algorithm


%%%===================================================================
%%% Internal functions
%%%===================================================================

integer_to_bit_string(Int) ->
    integer_to_bit_string(Int, "").

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
    
pad_to_length(BitString, ExpectedLength) when length(BitString) < ExpectedLength ->
    % Make a list of 0's of how short it is, and concatenate onto existing list
    lists:concat([[$0 || _ <- lists:seq(1, ExpectedLength - length(BitString))], BitString]);
pad_to_length(BitString, _ExpectedLength) ->
    BitString.

% This function, as the algorithm requires, assumes that each starting colour is unque, and will fail if not.
cole_vishkin_colour_reduction(_CurrentItem = #item{colour = Colour}, PreviousItemColour) ->
    convert_pair_to_binary_string(
        lists:foldl(
            fun({CurentColChar, PrevColChar}, Acc) ->
                compare_and_return(CurentColChar, PrevColChar, Acc)
            end,
            not_found,
            lists:zip(Colour, PreviousItemColour)
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
        {{#item{colour = "0110010000"}, "1010010000"}, "10001"},
        {{#item{colour = "1010010000"}, "0010110000"}, "1010"},
        {{#item{colour = "10001"}, "01010"}, "1"}
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
