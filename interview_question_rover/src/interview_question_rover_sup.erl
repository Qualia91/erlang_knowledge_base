%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application supervisor
%%% @author boc_dev
%%% @copyright MIT
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(interview_question_rover_sup).
-author(boc_dev).

-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    {ok, Pools} = application:get_env(interview_question_rover, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        WorkerImpl = proplists:get_value(worker_impl, WorkerArgs),
        PoolArgs = [{name, {local, Name}},
                    {worker_module, WorkerImpl}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {SupFlags, PoolSpecs}}.
