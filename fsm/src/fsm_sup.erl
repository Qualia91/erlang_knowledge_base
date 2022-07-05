%%%-------------------------------------------------------------------
%% @doc fsm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fsm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{
            id => light_switch,
            start => {light_switch, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
