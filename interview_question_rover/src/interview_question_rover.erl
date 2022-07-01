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

% interview_question_rover:send_data().

%% External API
-export([
    send_data/0
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

send_data() ->
    [StartPosLine | Rest] = readlines("input_file.txt"),
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

    lager:info("Return Data: ~p", [ReturnList]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
