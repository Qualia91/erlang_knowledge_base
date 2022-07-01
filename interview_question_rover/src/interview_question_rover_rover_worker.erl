%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(interview_question_rover_rover_worker).
-author(nickw).
-behaviour(poolboy_worker).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/1]).

%% Callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
    regex_pattern
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(map()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(map()) -> {ok, loop_state()}.
init(Args) ->
    process_flag(trap_exit, true),
    RegExp = "^\\((\\d*), (\\d*), ([NSEW])\\) (\\w*)$",
    {_, Pattern} = re:compile(RegExp),
    lager:debug("Rover worker started"),
    {ok, #loop_state{regex_pattern = Pattern}}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast({move_rover, From, WorkerIndex, StartX, StartY, InputLine}, LoopState = #loop_state{regex_pattern = Pattern}) ->
    lager:debug("Rover ~p recieved work - Start: (~p, ~p), Input: ~p", [WorkerIndex, StartX, StartY, InputLine]),
    From ! {rover_finished, WorkerIndex, InputLine},
    match_input_data(InputLine, Pattern),
    {noreply, LoopState};
handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
    ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal
%%%=============================================================================

match_input_data(InputData, Pattern) ->
    case re:run(InputData, Pattern) of
        {match, [_ | Groups]} -> 
            lager:info("Input data in rover recognised: ~p", [Groups]),
            parse_groups(Groups, InputData);
        nomatch -> 
            lager:error("Input data in rover not recognised"),
            {error, data_not_regognised}
    end.

parse_groups([StartX, StartY, StartOrientation, Actions], InputData) ->
    lager:info("StartX: ~p", [parse_group(StartX, InputData)]),
    lager:info("StartY: ~p", [parse_group(StartY, InputData)]),
    lager:info("StartOrientation: ~p", [parse_group(StartOrientation, InputData)]),
    lager:info("Actions: ~p", [parse_group(Actions, InputData)]).

parse_group({StartPos, Size}, InputData) ->
    lists:sublist(InputData, StartPos + 1, Size).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.			
