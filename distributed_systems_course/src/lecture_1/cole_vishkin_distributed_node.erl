%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(cole_vishkin_distributed_node).
-author(boc_dev).
-behaviour(gen_distributed_node).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/3,
    request_some_data/2,
    start_iteration/1,
    update_colours/1
]).

%% Callbacks
-export([
    init/1,
    intercept_request/2,
    handle_request_data/2,
    handle_response_data/2,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

-record(loop_state, {
    index        :: integer(),
    colour       :: string(),
    new_colour   :: string(),
    database_pid :: pid()
}).
-opaque loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Index, BitStringLength, DatabasePid) ->
    gen_distributed_node:start_link(?MODULE, Index, [Index, BitStringLength, DatabasePid]).

request_some_data(NodePid, Request) ->
    gen_distributed_node:request_data(NodePid, Request).

start_iteration(Pid) ->
    gen_distributed_node:request_data(Pid, iterate).

update_colours(Pid) ->
    gen_server:cast(Pid, update_colours).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

init([Index, BitStringLength, DatabasePid]) ->
    StartingColour = cole_vishkin:pad_to_length(cole_vishkin:integer_to_bit_string(Index), BitStringLength),
    #loop_state{
        index        = Index,
        colour       = StartingColour,
        database_pid = DatabasePid
    }.

intercept_request(iterate, State) ->
    {request_colours, State}.

handle_request_data(request_colours, State = #loop_state{colour = Colour}) ->
    {Colour, State}.

handle_response_data(ParentColours, State = #loop_state{colour = Colour, index = Index, database_pid = DatabasePid}) ->
    NewColour = cole_vishkin:cole_vishkin_colour_reduction(Colour, ParentColours),
    DatabasePid ! {update_colour, Index, NewColour},
    State#loop_state{new_colour = NewColour}.

handle_cast(update_colours, LoopState = #loop_state{new_colour = NewColour}) ->
    {noreply, LoopState#loop_state{colour = NewColour}}.

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
