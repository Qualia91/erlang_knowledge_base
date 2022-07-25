%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(dist_patterns_sup).
-author(boc_dev).
-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [
        create_child(db_cluster_sup,       db_cluster_sup,       [],  supervisor),
        create_child(cluster_monitor_serv, cluster_monitor_serv, [5], worker),

        create_child(write_ahead_log,      write_ahead_log,      ["MyDatabase"], worker)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal
%%%=============================================================================

create_child(ID, Module, Inputs, Type) ->
    #{
        id => ID,
        start => {Module, start_link, Inputs},
        restart => permanent,
        shutdown => brutal_kill,
        type => Type
    }.