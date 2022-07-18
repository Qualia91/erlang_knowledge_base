%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(gen_distributed_node).
-author(boc_dev).
-behaviour(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start_link/3,
    set_neighbours/2,
    request_data/2
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

-type neighbour_object(StoredDataType) :: {pid(), StoredDataType}.

%% Loop state
-record(loop_state, {
    module                 :: atom(),
    unique_index           :: integer(),
    neighbour_objects = [] :: list(neighbour_object(any())),
    user_data              :: any()
}).
-opaque loop_state() :: loop_state.

%%%=============================================================================
%%% Callback definitions
%%%=============================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.

-callback request_data(State :: any()) -> {Request :: any(), State :: any()}.

-callback respond_data(State :: any()) -> {Response :: any(), State :: any()}.

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Module, Index, Data) ->
    gen_server:start_link(?MODULE, [Module, Index, Data], []).

set_neighbours(Pid, NeighbourPids) ->
    gen_server:cast(Pid, {set_neighbours, NeighbourPids}).

request_data(NodePid, Request) ->
    gen_server:cast(NodePid, {request_data, Request}).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

-spec init(list()) -> {ok, loop_state()}.
init([Module, Index, Data]) ->
    LoopState = #loop_state{
        module       = Module,
        unique_index = Index,
        user_data    = Data
    },
    lager:debug("Starting node ~p", [Index]),
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(Request, _From, LoopState = #loop_state{module = Module}) ->
    try
        Module:handle_call(Request, _From, LoopState)
    catch
        error:undef ->
            Reply = ok,
            lager:debug("Other handle_call: ~p~n", [Request]),
            {reply, Reply, LoopState}
    end.

handle_cast({set_neighbours, NeighbourPids}, LoopState) ->
    {noreply, LoopState#loop_state{neighbour_objects = create_neighbour_objs(NeighbourPids)}};
handle_cast({request_data, Request}, LoopState = #loop_state{user_data = UserData, module = Module, neighbour_objects = NeighbourObjs}) ->
    {Request, UpdatedUserState} = try
        Module:request_data(Request, UserData)
    catch
        error:undef ->
            lager:debug("Other request_data: ~p", [Request]),
            {Request, UserData}
    end,
    request_and_reset_neighbour_data(Module:request_data(), NeighbourObjs);
    {noreply, LoopState#loop_state{user_data = UpdatedUserState}};
handle_cast({request_data, Request, From}, LoopState = #loop_state{user_data = UserData, module = Module, neighbour_objects = NeighbourObjs}) ->
    {Response, UpdatedUserState} = Module:response_data(Request, UserData),
    respond_data(From, Response),
    {noreply, LoopState#loop_state{user_data = UpdatedUserState}};
handle_cast(Msg, LoopState = #loop_state{module = Module}) ->
    try
        Module:handle_cast(Msg, LoopState)
    catch
        error:undef ->
            lager:debug("Other handle_cast: ~p~n", [Msg]),
            {noreply, LoopState}
    end.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(Info, LoopState = #loop_state{module = Module}) ->
    try
        Module:handle_info(Info, LoopState)
    catch
        error:undef ->
            lager:debug("Other handle_info: ~p~n", [Info]),
            {noreply, LoopState}
    end.

-spec terminate(any(), loop_state()) -> ok.
terminate(Reason, LoopState = #loop_state{module = Module}) ->
    try
        Module:terminate(Reason, LoopState)
    catch
        error:undef ->
            lager:debug("Terminating~n")
    end.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(OldVsn, LoopState = #loop_state{module = Module}, Extra) ->
    try
        Module:code_change(OldVsn, LoopState, Extra)
    catch
        error:undef ->
            lager:debug("Code Changing"),
            {ok, LoopState}
    end.


%%%=============================================================================
%%% Internal
%%%=============================================================================

request_data(NodePid, Request, ReturnAddress) ->
    gen_server:cast(NodePid, {request_data, Request, ReturnAddress}).

respond_data(NodePid, Response) ->
    gen_server:cast(NodePid, {respond_data, Response}).

create_neighbour_objs(Pids) ->
    lists:map(
        fun(Pid) ->
            {Pid, null}
        end,
        Pids).

request_and_reset_neighbour_data(Request, NeighbourObjs) ->
    lists:map(
        fun({Pid, _}) ->
            request_data(Pid, Request, self()),
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