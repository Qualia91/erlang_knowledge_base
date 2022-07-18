%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(my_distributed_node).
-author(boc_dev).
-behaviour(gen_distributed_node).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/1,
    request_some_data/1
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Index) ->
    gen_distributed_node:start_link(?MODULE, Index, []).

request_some_data(NodePid, Request) ->
    gen_distributed_node:request_data(NodePid, Request).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

init([]) ->
    {ok, []}.

request_data(some_req, State) ->
    {some_req, State}.

respond_data(give_me_data, NeighbourData, State) ->
    {hello, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
