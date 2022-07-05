%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(server).
-author(nickw).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0, call_this/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

call_this() ->
    gen_server:call(?SERVER, req).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([]) ->
    LoopState = #loop_state{}, 
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(req, _From, LoopState) ->
    Reply = <<"Stuff">>,
    {reply, Reply, LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.
