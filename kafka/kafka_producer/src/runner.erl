%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author bocdev
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(runner).
-author(bocdev).
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0]).

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

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([]) ->
    LoopState = #loop_state{},
    my_publisher:start(<<"my_topic">>, #{}),
    erlang:send_after(1000, ?SERVER, publish),
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.


-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(publish, LoopState) ->
    my_publisher:publish(<<"my_topic">>, 0, <<"time_now">>, time_to_binary()),
    erlang:send_after(1000, ?SERVER, publish),
    {noreply, LoopState};
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

time_to_binary() ->
    {Y,M,D} = erlang:timestamp(),
    BinY = list_to_binary(integer_to_list(Y)),
    BinM = list_to_binary(integer_to_list(M)),
    BinD = list_to_binary(integer_to_list(D)),
    <<BinY/binary, BinM/binary, BinD/binary>>.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
