%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(cluster_monitor_serv).
-author(nickw).
-behaviour(gen_server).

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
    db_pids :: list(pid()),
    cluster_size :: integer()
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(integer()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(ClusterSize) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ClusterSize], []).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([ClusterSize]) ->

    gen_server:cast(self(), startup),

    {ok, #loop_state{cluster_size = ClusterSize}}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.


-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast(startup, LoopState = #loop_state{cluster_size = ClusterSize}) ->

    DBPids = lists:foldl(
        fun(Index, Acc) ->
            [create_db_node(integer_to_binary(Index)) | Acc]
        end,
        [],
        lists:seq(1, ClusterSize) 
    ),

    UpdatedLoopState = LoopState#loop_state{
        db_pids = DBPids
    },

    {noreply, UpdatedLoopState};
handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info({'DOWN', Reference, process, Pid, {error, _Reason, DBName}}, LoopState = #loop_state{db_pids = DBPids}) ->
    lists:foldl(
        fun(Elem, Acc) -> db_down_handler(Elem, Reference, Pid, DBName, Acc) end,
        [],
        DBPids
    ),
    {noreply, LoopState};
handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
    ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

create_db_child(ID, Module, Inputs, Type) ->
    #{
        id => ID,
        start => {Module, start_link, Inputs},
        restart => temporary,
        shutdown => brutal_kill,
        type => Type
    }.

db_down_handler({Ref, Pid}, Ref, Pid, DBName, Acc) ->
    [create_db_node(DBName) | Acc];
db_down_handler(_, Ref, Pid, _, Acc) ->
    [{Ref, Pid} | Acc].

create_db_node(DBName) ->
    {ok, Pid} = supervisor:start_child(
        db_cluster_sup, 
        create_db_child(DBName, db_node, [DBName], worker)
    ),
    Ref = erlang:monitor(process, Pid),
    {Ref, Pid}.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
