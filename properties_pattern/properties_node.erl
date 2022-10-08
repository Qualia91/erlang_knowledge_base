%%%-----------------------------------------------------------------------------
%%% @doc
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(properties_node).
-author("nickolaswood").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/1,
    set_parent/2,
    get_env/2,
    set_env/3
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
    parent = null,
    env
}).

-define(TIMEOUT, 5000).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(InitialEnv) ->
    gen_server:start_link(?MODULE, InitialEnv, []).

set_parent(Pid, ParentPid) ->
    gen_server:cast(Pid, {set_parent, ParentPid}).

get_env(null, _) ->
    {error, not_found};
get_env(Pid, EnvPath) ->
    gen_server:call(Pid, {get_env, EnvPath}, ?TIMEOUT).

set_env(Pid, EnvPath, Var) ->
    gen_server:cast(Pid, {set_env, EnvPath, Var}).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init(InitialEnv) ->
    LoopState = #loop_state{
        env = InitialEnv
    }, 
    {ok, LoopState}.

handle_call({get_env, EnvPath}, _From, LoopState = #loop_state{env = Env, parent = Parent}) ->
    Resp = case find_val_in_map(Env, EnvPath) of
        {error, not_found} ->
            get_env(Parent, EnvPath);
        Val ->
            Val
    end,
    {reply, Resp, LoopState}.

handle_cast({set_parent, ParentPid}, LoopState) ->
    {noreply, LoopState#loop_state{parent = ParentPid}};
handle_cast({set_env, EnvPath, Var}, LoopState = #loop_state{env = Env}) ->
    {noreply, LoopState#loop_state{env = set_val_in_env(Env, EnvPath, Var)}}.

handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

find_val_in_map(Env, []) ->
    Env;
find_val_in_map(Env, [CurrentEnvKey | RestEnvKeys]) ->
    case maps:find(CurrentEnvKey, Env) of
        {ok, SubEnv} ->
            find_val_in_map(SubEnv, RestEnvKeys);
        error ->
            {error, not_found}
    end;
find_val_in_map(Env, Key) ->
    find_val_in_map(Env, [Key]).

set_val_in_env(Env, [Key], Var) ->
    Env#{Key => Var};
set_val_in_env(Env, [CurrentEnvKey | RestEnvKeys], Var) ->
    case maps:find(CurrentEnvKey, Env) of
        {ok, SubEnv} when is_map(SubEnv) ->
            Env#{CurrentEnvKey => set_val_in_env(SubEnv, RestEnvKeys, Var)};
        error ->
            {error, not_found}
    end;
set_val_in_env(Env, Key, Var) ->
    set_val_in_env(Env, [Key], Var).

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.