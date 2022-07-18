%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(distributed_node).
-author(boc_dev).
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/3,
    start_iteration/1,
    request_colour/2,
    respond_colour/3,
    set_neighbours/2,
    update_colours/1
]).

%% Callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

-define(SERVER, ?MODULE).

-opaque neighbour_object() :: {pid(), string()}.

%% Loop state
-record(loop_state, {
    index                  :: integer(),
    neighbour_objects = [] :: list(neighbour_object()),
    colour                 :: string(),
    new_colour             :: string(),
    database_pid           :: pid()
}).
-opaque loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(integer(), integer(), pid()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Index, BitStringLength, DatabasePid) ->
    gen_server:start_link(?MODULE, [Index, BitStringLength, DatabasePid], []).

start_iteration(Pid) ->
    gen_server:cast(Pid, iterate).

request_colour(Pid, ReturnAddress) ->
    gen_server:cast(Pid, {request_colour, ReturnAddress}).

respond_colour(Pid, NeighbourColour, NeighbourPid) ->
    gen_server:cast(Pid, {respond_colour, NeighbourColour, NeighbourPid}).

set_neighbours(Pid, NeighbourPids) ->
    gen_server:cast(Pid, {set_neighbours, NeighbourPids}).

update_colours(Pid) ->
    gen_server:cast(Pid, update_colours).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([Index, BitStringLength, DatabasePid]) ->
    StartingColour = cole_vishkin:pad_to_length(cole_vishkin:integer_to_bit_string(Index), BitStringLength),
    LoopState = #loop_state{
        index             = Index,
        colour            = StartingColour,
        database_pid      = DatabasePid
    },
    lager:debug("Starting node ~p with colour ~p", [Index, StartingColour]),
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(Request, _From, LoopState) ->
    Reply = ok,
    lager:debug("Other handle_call: ~p~n", [Request]),
    {reply, Reply, LoopState}.

-spec handle_cast(any(), loop_state()) -> {noreply, loop_state()}.
handle_cast({set_neighbours, NeighbourPids}, LoopState) ->
    {noreply, LoopState#loop_state{neighbour_objects = create_neighbour_objs(NeighbourPids)}};
handle_cast({request_colour, ReturnAddress}, LoopState = #loop_state{colour = Colour}) ->
    distributed_node:respond_colour(ReturnAddress, Colour, self()),
    {noreply, LoopState};
handle_cast({respond_colour, NeighbourColour, NeighbourPid}, LoopState = #loop_state{neighbour_objects = NeighbourObjs}) ->
    {IsComplete, UpdatedNeighbourObjs} = update_and_check_complete(NeighbourColour, NeighbourPid, NeighbourObjs),
    start_processing(IsComplete),
    {noreply, LoopState#loop_state{neighbour_objects = UpdatedNeighbourObjs}};
handle_cast(start_processing, LoopState = #loop_state{index = Index, colour = Colour, neighbour_objects = NeighbourObjs, database_pid = DatabasePid}) ->
    NewColour = cole_vishkin:cole_vishkin_colour_reduction(Colour, get_neighbour_colours(NeighbourObjs)),
    DatabasePid ! {update_colour, Index, NewColour},
    {noreply, LoopState#loop_state{new_colour = NewColour}};
handle_cast(iterate, LoopState = #loop_state{neighbour_objects = NeighbourObjs}) ->
    {noreply, LoopState#loop_state{neighbour_objects = request_and_reset_neighbour_colours(NeighbourObjs)}};
handle_cast(update_colours, LoopState = #loop_state{new_colour = NewColour}) ->
    io:format("HERE: ~p~n", [LoopState]),
    {noreply, LoopState#loop_state{colour = NewColour}};
handle_cast(Msg, LoopState) ->
    lager:debug("Other handle_cast: ~p~n", [Msg]),
    {noreply, LoopState}.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(Info, LoopState) ->
    lager:debug("Other handle_info: ~p~n", [Info]),
    {noreply, LoopState}.

-spec terminate(any(), loop_state()) -> ok.
terminate(_Reason, _LoopState) ->
    lager:debug("Terminating~n"),
    ok.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.


%%%=============================================================================
%%% Internal
%%%=============================================================================

create_neighbour_objs(Pids) ->
    lists:map(
        fun(Pid) ->
            {Pid, null}
        end,
        Pids).

request_and_reset_neighbour_colours(NeighbourObjs) ->
    lists:map(
        fun({Pid, _}) ->
            distributed_node:request_colour(Pid, self()),
            {Pid, null}
        end,
        NeighbourObjs).

update_and_check_complete(NeighbourColour, NeighbourPid, NeighbourObjs) ->
    lists:foldl(
        fun(Elem, Acc) -> 
            update_and_check_iter(Elem, Acc, NeighbourColour, NeighbourPid) 
        end,
        {true, []},
        NeighbourObjs).

update_and_check_iter({Pid, _IterColour}, {IsComplete, Acc}, NeighbourColour, Pid) ->
    {IsComplete, [{Pid, NeighbourColour} | Acc]};
update_and_check_iter({IterPid, null}, {_IsComplete, Acc}, _NeighbourColour, _NeighbourPid) ->
    {false, [{IterPid, null} | Acc]};
update_and_check_iter({IterPid, IterColour}, {IsComplete, Acc}, _NeighbourColour, _NeighbourPid) ->
    {IsComplete, [{IterPid, IterColour} | Acc]}.

start_processing(true) ->
    gen_server:cast(self(), start_processing);
start_processing(false) ->
    ok.

get_neighbour_colours(NeighbourColours) ->
    {_, Colours} = lists:unzip(NeighbourColours),
    Colours.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

create_neighbour_objs_test() ->
    
    TestCases = [
        {[1,2,3], [{1,null}, {2,null}, {3,null}]},
        {[], []}
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> create_neighbour_objs(Input) end).

request_and_reset_neighbour_colours_test() ->
    
    meck:new(distributed_node, [unstick, passthrough]),
    meck:expect(distributed_node, request_colour, fun(_, _) -> ok end),
    
    TestCases = [
        {[{1,1}, {2,2}, {3,3}], [{1,null}, {2,null}, {3,null}]},
        {[], []}
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> request_and_reset_neighbour_colours(Input) end),

    meck:unload(distributed_node).

update_and_check_iter_test() ->
    
    % Test cases
    % 1) All are complete so far. Input matches. Returns is complete with updated array
    % 2) It isnt complete so far. Input matches. Returns isnt complete with updated array
    % 3) All are complete so far. Input doesnt match. Returns isnt complete with same array
    % 4) It isnt complete so far. Input doesnt match. Returns isnt complete with same array

    TestCases = [
        {{{1, null}, {true,  [     ]}, 1, 1}, {true,  [{1, 1   }        ]}},
        {{{1, null}, {false, [     ]}, 1, 1}, {false, [{1, 1   }        ]}},
        {{{1, null}, {true,  [     ]}, 1, 2}, {false, [{1, null}        ]}},
        {{{1, null}, {false, [     ]}, 1, 2}, {false, [{1, null}        ]}},

        {{{1, null}, {false, [{2,2}]}, 1, 2}, {false, [{1, null}, {2, 2}]}},
        {{{1, null}, {false, [{2,2}]}, 1, 1}, {false, [{1, 1   }, {2, 2}]}},
        {{{1, null}, {true,  [{2,2}]}, 1, 2}, {false, [{1, null}, {2, 2}]}},
        {{{1, null}, {true,  [{2,2}]}, 1, 1}, {true,  [{1, 1   }, {2, 2}]}},

        {{{2, 2   }, {true,  [{1,1}]}, 1, 1}, {true,  [{2, 2   }, {1, 1}]}}
    ],

    test_utilities:action_test_cases(TestCases, fun({IterPair, AccPair, NeighbourColour, Pid}) -> update_and_check_iter(IterPair, AccPair, NeighbourColour, Pid) end).

update_and_check_complete_test() ->
    
    % Test cases
    % 1) Inputs list with all missing and no matches. Returns not complete with the original array
    % 2) Inputs list with all missing and one matches. Returns not complete with the updated array
    % 2) Inputs list with one missing and one matches. Returns complete with the updated array

    TestCases = [
        {
            {
                0,
                0,
                [{1, null}, {2, null}, {3, null}]
            },
            {false, [{3, null}, {2, null}, {1, null}]}
        },
        {
            {
                1,
                1,
                [{1, null}, {2, null}, {3, null}]
            },
            {false, [{3, null}, {2, null}, {1, 1}]}
        },
        {
            {
                1,
                1,
                [{1, null}, {2, 2}, {3, 3}]
            },
            {true, [{3, 3}, {2, 2}, {1, 1}]}
        },
        {
            {
                1,
                1,
                [{1, null}, {2, null}, {3, 3}]
            },
            {false, [{3, 3}, {2, null}, {1, 1}]}
        }
    ],

    test_utilities:action_test_cases(TestCases, fun({NeighbourColour, NeighbourPid, NeighbourObjs}) -> update_and_check_complete(NeighbourColour, NeighbourPid, NeighbourObjs) end).

get_neighbour_colours_test() ->
    
    TestCases = [
        {[{a,1}, {b,2}, {c,3}], [1,2,3]},
        {[], []}
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> get_neighbour_colours(Input) end).

-endif.