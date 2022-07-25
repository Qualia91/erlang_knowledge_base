%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(db_node).
-author(boc_dev).
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/1,
    set/3,
    get/2,
    delete/2,
    cause_crash/1
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

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
    database_name :: string(),
    kv_store      :: ets:tid()
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

% write_ahead_log:set(a, b).
% write_ahead_log:set(b, b).
% write_ahead_log:get(a).
% write_ahead_log:delete(a).
% write_ahead_log:get(a).
% write_ahead_log:get(b).
% write_ahead_log:cause_crash().
% write_ahead_log:get(b).

-spec start_link(string()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(DBName) ->
    gen_server:start_link(?MODULE, [DBName], []).

set(Server, Key, Value) ->
    gen_server:cast(Server, {set, Key, Value}).

get(Server, Key) ->
    gen_server:call(Server, {get, Key}).

delete(Server, Key) ->
    gen_server:cast(Server, {delete, Key}).

cause_crash(Server) ->
    gen_server:call(Server, wrong_message).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([DBName]) ->
    Tid = ets:new(tab, [private, bag]),
    LoopState = #loop_state{
        database_name = DBName,
        kv_store      = Tid
    },
    case file:read_file(DBName) of
        {ok, ContentsBinary} ->
            apply_history(ContentsBinary, Tid);
        {error, enoent} ->
            create_write_ahead_file(DBName)
    end,
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call({get, Key}, _From, LoopState = #loop_state{kv_store = Tid}) ->
    {reply, ets:lookup(Tid, Key), LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast(Command, LoopState = #loop_state{database_name = DBName, kv_store = Tid}) ->
    BinaryCommand = term_to_binary(Command),
    write_command_to_file(BinaryCommand, DBName),
    apply_command(BinaryCommand, Tid),
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
%%% Internal functions
%%%=============================================================================

apply_history(ContentsBinary, Tid) ->
    lists:foreach(
        fun(Command) ->
            apply_command(Command, Tid)
        end,
        binary:split(ContentsBinary, [<<"\n">>], [global])
    ).

create_write_ahead_file(DBName) ->
    file:write_file(DBName, "").

write_command_to_file(Command, DBName) ->
    file:write_file(DBName, Command, [append]),
    file:write_file(DBName, "\n", [append]).

apply_command(<<>>, _) ->
    ok;
apply_command(Command, Tid) ->
    case binary_to_term(Command) of
        {set, Key, Value} ->
            ets:insert(Tid, {Key, Value});
        {delete, Key} ->
            ets:delete(Tid, Key);
        _ ->
            ok
    end.
    
%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
