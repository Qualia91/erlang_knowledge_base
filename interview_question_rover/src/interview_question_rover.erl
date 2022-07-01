%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(interview_question_rover).
-author(nickw).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-define(COLLECT_DATA_TIMEOUT, 1000).

%% External API
-export([
    read_input_and_run/0,
    read_input_and_run/1
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

read_input_and_run() ->
    read_input_and_run("input_file.txt").

read_input_and_run(FileName) ->
    [StartPosLine | Rest] = readlines(FileName),
    [StartXStr, StartYStr] = string:tokens(StartPosLine, " "),
    StartX = list_to_integer(StartXStr),
    StartY = list_to_integer(StartYStr),

    NoRoversRunning = lists:foldl(
        fun(Line, Index) ->
            poolboy:transaction(rover_workers, fun(Worker) ->
                gen_server:cast(Worker, {move_rover, self(), Index, StartX, StartY, Line})
            end),
            Index + 1
        end,
        0,
        Rest),

    lager:info("Rovers running: ~p", [NoRoversRunning]),
    
    {_, ReturnList} = lists:unzip(lists:sort(collect_data(NoRoversRunning, 0, []))),
    
    print_all(ReturnList).

%%%===================================================================
%%% Internal functions
%%%===================================================================

print_all([]) ->
    ok;
print_all([Hd | Rest]) ->
    print(Hd),
    print_all(Rest).

print({lost, {transform, X, Y, Ori}}) ->
    io:format("(~p, ~p, ~c) ~s~n", [X, Y, Ori, "LOST"]);
print({not_lost, {transform, X, Y, Ori}}) ->
    io:format("(~p, ~p, ~c)~n", [X, Y, Ori]).

readlines(FileName) ->
    {ok, Binary} = file:read_file(FileName),
    string:tokens(erlang:binary_to_list(Binary), "\n\r").

collect_data(NoRoversRunning, ResponseSize, Acc) when NoRoversRunning == ResponseSize ->
    Acc;
collect_data(NoRoversRunning, ResponseSize, Acc) ->
    CollectedData = 
        receive
            {rover_finished, Index, Resp} ->
                [{Index, Resp} | Acc]
        after 
            ?COLLECT_DATA_TIMEOUT ->
                [{error, response_timeout} | Acc] 
        end,
    collect_data(NoRoversRunning, ResponseSize + 1, CollectedData).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.
