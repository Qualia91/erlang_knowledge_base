%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application supervisor
%%% @author nickolas.wood
%%% @date 2022-08-11
%%% @end
%%%-----------------------------------------------------------------------------

-module(cowboy_optional_route_sup).
-author("nickolas.wood").

-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[:prop]/authenticate", cowboy_static, {priv_file, cowboy_optional_route, "test_page.html"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, 8081}
        ],
        #{env=>#{dispatch=>Dispatch}
    }),

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).    

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                    intensity => 1,
                    period => 5},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.