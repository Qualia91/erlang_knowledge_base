%%%-----------------------------------------------------------------------------
%%% @doc
%%% Properties pattern
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(properties_pattern).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    properties_patter_example/0
]).

% c(properties_node).
% c(properties_pattern).
% properties_pattern:properties_patter_example().

%%%=============================================================================
%%% API
%%%=============================================================================

properties_patter_example() ->

    GlobalEnvInit = #{
        a => 1,
        b => 2,
        c => 3
    },

    Level1EnvInit = #{
        a => 2,
        d => 3
    },

    Level2EnvInit = #{
        b => 7,
        e => 3
    },

    {ok, GlobalEnv} = properties_node:start_link(GlobalEnvInit),
    {ok, Level1Env} = properties_node:start_link(Level1EnvInit),
    {ok, Level2Env} = properties_node:start_link(Level2EnvInit),

    properties_node:set_parent(Level2Env, Level1Env),
    properties_node:set_parent(Level1Env, GlobalEnv),

    A = properties_node:get_env(Level2Env, a),
    B = properties_node:get_env(Level2Env, b),
    C = properties_node:get_env(Level2Env, c),
    D = properties_node:get_env(Level2Env, d),
    E = properties_node:get_env(Level2Env, e),
    G = properties_node:get_env(Level2Env, []),

    io:format("~p, ~p, ~p, ~p, ~p, ~p~n", [A, B, C, D, E, G]).
    

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
