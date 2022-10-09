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

%% Loop state
-record(loop_state, {
    parent = null :: null | pid(),
    env           :: maps:map()
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
        Val when is_map(Val) ->
            combine_envs(Val, get_env(Parent, EnvPath));
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
        _ ->
            Env#{CurrentEnvKey => set_val_in_env(#{}, RestEnvKeys, Var)}
    end;
set_val_in_env(Env, Key, Var) ->
    set_val_in_env(Env, [Key], Var).

combine_envs(CurrentEnv, {error, not_found}) ->
    CurrentEnv;
combine_envs(CurrentEnv, ParentEnv) when is_map(ParentEnv) ->
    maps:merge_with(fun env_combiner/3, ParentEnv, CurrentEnv);
combine_envs(CurrentEnv, _ParentEnvVal) ->
    CurrentEnv.

env_combiner(_Key, Env1, Env2) when is_map(Env1), is_map(Env2) ->
    maps:merge_with(fun env_combiner/3, Env1, Env2);
env_combiner(_Key, _Env1, Env2) ->
    Env2.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

% -include_lib("eunit/include/eunit.hrl").
% c(properties_node).
% properties_node:find_val_in_map_test().
% properties_node:set_val_in_env_test().
% properties_node:combine_envs_simple_test().
% properties_node:combine_envs_not_found_test().
% properties_node:combine_envs_complex_test().
% properties_node:env_combiner_test().

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_val_in_map_test() ->

    Env = #{
        a => #{
            b => 2,
            c => 3
        },
        d => 4
    },

    ?assertEqual(Env, find_val_in_map(Env, [])),
    ?assertEqual(#{b => 2, c => 3}, find_val_in_map(Env, a)),
    ?assertEqual(2, find_val_in_map(Env, [a, b])),
    ?assertEqual(3, find_val_in_map(Env, [a, c])),
    ?assertEqual(4, find_val_in_map(Env, d)),
    ?assertEqual({error, not_found}, find_val_in_map(Env, e)).

set_val_in_env_test() ->

    Env = #{
        a => #{
            b => 2,
            c => 3
        },
        d => 4
    },

    ?assertEqual(#{a => #{b => 2, c => 3}, d => 5}, set_val_in_env(Env, d, 5)),
    ?assertEqual(#{a => #{b => 5, c => 3}, d => 4}, set_val_in_env(Env, [a, b], 5)),
    ?assertEqual(#{a => #{b => 2, c => #{e => 5}}, d => 4}, set_val_in_env(Env, [a, c, e], 5)),
    ?assertEqual(#{a => #{b => 2, c => 3, d => #{e => 5}}, d => 4}, set_val_in_env(Env, [a, d, e], 5)).

combine_envs_simple_test() ->

    CurrentEnv = #{
        a => #{
            b => 2,
            c => 3
        },
        d => 4
    },

    ParentEnv = #{
        a => 5,
        d => #{a => 5},
        e => 4
    },

    ExpectedEnv = #{
        a => #{
            b => 2,
            c => 3
        },
        d => 4,
        e => 4
    },

    ?assertEqual(ExpectedEnv, combine_envs(CurrentEnv, ParentEnv)).

combine_envs_not_found_test() ->

    CurrentEnv = #{
        a => #{
            b => 2,
            c => 3
        },
        d => 4
    },

    ParentEnv = {error, not_found},

    ?assertEqual(CurrentEnv, combine_envs(CurrentEnv, ParentEnv)).

combine_envs_complex_test() ->

    CurrentEnv = #{
        a => #{
            b => 2,
            c => 3,
            h => #{
                a => 1
            }
        },
        d => 4
    },

    ParentEnv = #{
        a => #{
            f => 2,
            g => 3,
            h => #{
                b => 1
            }
        }
    },

    ExpectedEnv = #{
        a => #{
            b => 2,
            c => 3,
            f => 2,
            g => 3,
            h => #{
                a => 1,
                b => 1
            }
        },
        d => 4
    },

    ?assertEqual(ExpectedEnv, combine_envs(CurrentEnv, ParentEnv)).

env_combiner_test() ->

    CurrentEnv = #{
        b => 2,
        c => 3
    },

    ParentEnv = #{
        f => 2,
        g => 3
    },

    ExpectedEnv = #{
        b => 2,
        c => 3,
        f => 2,
        g => 3
    },

    ?assertEqual(ExpectedEnv, env_combiner(a, ParentEnv, CurrentEnv)).

-endif.