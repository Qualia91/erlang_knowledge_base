%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen State Machine
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(light_switch).
-author(nickw).
-behaviour(gen_statem).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/0,
    stop/0,
    push/0
]).

%% Gen State Machine Callbacks
-export([
    init/1,
    code_change/4,
    callback_mode/0,
    terminate/3
]).

%% State transitions
-export([
    off/3,
    on/3,
    handle_event/3
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
    gen_statem:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?SERVER).

push() ->
    gen_statem:call(?SERVER, push).

%%%=============================================================================
%%% Gen State Machine Callbacks
%%%=============================================================================

init([]) ->
    State = off,
    Data = data, 
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
    
callback_mode() -> 
    state_functions.

%%%=============================================================================
%%% State transitions
%%%=============================================================================

off({call,From}, push, Data) ->
    {next_state, on, Data, [{reply,From,on}]};
    off(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

on({call,From}, push, Data) ->
    {next_state, off, Data, [{reply,From,off}]};
    on(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%%-----------------------------------------------------------------------------
%% @doc
%% Handle events common to all states 
%% @end
%%-----------------------------------------------------------------------------
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
