%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(messaging_distributed_node).
-author(boc_dev).
-behaviour(gen_distributed_node).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/1,
    request_some_data/2,
    send_some_data/2
]).

%% Callbacks
-export([
    init/1,
    intercept_request/2,
    intercept_send_data/2,
    handle_request_data/2,
    handle_response_data/2,
    handle_incoming_data/2
]).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Index) ->
    gen_distributed_node:start_link(?MODULE, Index, []).

request_some_data(NodePid, Request) ->
    gen_distributed_node:request_data(NodePid, Request).

send_some_data(NodePid, Request) ->
    gen_distributed_node:send_data(NodePid, Request).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

init([]) ->
    [].

intercept_request(some_req, State) ->
    io:format("intercept_request"),
    {give_me_data, State}.

handle_request_data(give_me_data, State) ->
    io:format("handle_request_data"),
    {hello, State}.

handle_response_data(NeighbourData, State) ->
    io:format("handle_response_data: ~p", [NeighbourData]),
    State.

intercept_send_data(some_data, State) ->
    io:format("intercept_data"),
    {here_is_some_data, State}.

handle_incoming_data(IncomingData, State) ->
    io:format("handle_incoming_data ~p", [IncomingData]),
    State.

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
