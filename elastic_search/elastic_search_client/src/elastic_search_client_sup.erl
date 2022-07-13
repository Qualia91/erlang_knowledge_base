%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application supervisor
%%% @author boc_dev
%%% @copyright Apache 2.0
%%% @version 0.1.0
%%% @date 2022-07-06
%%% @end
%%%-----------------------------------------------------------------------------

-module(elastic_search_client_sup).
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

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},

    ChildSpecs = [

    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_child(ID, Module, Inputs, Type) ->
    #{
        id => ID,
        start => {Module, start_link, Inputs},
        restart => permanent,
        shutdown => brutal_kill,
        type => Type
    }.