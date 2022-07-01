%%%-------------------------------------------------------------------
%% @doc interview_question_rover top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(interview_question_rover_sup).

-behaviour(supervisor).

-export([
    start_link/0
]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
