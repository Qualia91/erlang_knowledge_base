%%%-----------------------------------------------------------------------------
%%% @doc
%%% Properties pattern
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(properties_pattern).
-author("nickolaswood").

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
properties_pattern_simple_example/0
]).

% c(properties_node).
% c(properties_pattern).
% properties_pattern:properties_pattern_simple_example().

%%%=============================================================================
%%% API
%%%=============================================================================

properties_pattern_simple_example() ->

    GlobalEnvInit = #{
        a => 1,
        b => 2,
        c => 3,
        x => #{
            a => 1,
            c => 3
        }
    },

    Level1EnvInit = #{
        a => 2,
        d => 3,
        x => #{
            a => 2
        }
    },

    Level2EnvInit = #{
        b => 7,
        e => 3,
        x => #{
            b => 1
        }
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
    F = properties_node:get_env(Level2Env, f),
    G = properties_node:get_env(Level2Env, []),
    H = properties_node:get_env(Level2Env, [x, a]),
    I = properties_node:get_env(Level2Env, x),

    ?assertEqual(2, A),
    ?assertEqual(7, B),
    ?assertEqual(3, C),
    ?assertEqual(3, D),
    ?assertEqual(3, E),
    ?assertEqual({error, not_found}, F),

    Expected = #{
        a => 2,
        b => 7,
        c => 3,
        d => 3,
        e => 3,
        x => #{
            a => 2,
            b => 1,
            c => 3
        }
    },

    ?assertEqual(Expected, G),
    ?assertEqual(2, H),
    ?assertEqual(#{a => 2, b => 1, c => 3}, I).
    

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
