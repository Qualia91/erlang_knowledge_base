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
    module                    :: atom(),
    unique_index              :: integer(),
    neighbour_pids = []       :: list(pid()),
    user_data                 :: any(),
    request_data_struct = #{} :: map()
}).
-opaque loop_state() :: loop_state.

%%%=============================================================================
%%% Callback definitions
%%%=============================================================================

-callback init(Args :: term()) -> State :: term().

-callback intercept_request(InputRequest :: any(), State :: any()) -> {UpdatedRequest :: any(), State :: any()}.

-callback handle_request_data(Request :: any(), State :: any()) -> {Response :: any(), State :: any()}.

-callback handle_response_data(RequestedData :: list(any()), State :: any()) -> State :: any().

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
        user_data    = Module:init(Data)
    },
    lager:debug("Starting node ~p", [Index]),
    {ok, LoopState}.

-spec handle_call(any(), pid(), loop_state()) -> {ok, any(), loop_state()}.
handle_call(Request, From, LoopState = #loop_state{module = Module, user_data = Data}) ->
    try
        {RespType, Reply, UpdatedData} = Module:handle_call(Request, From, Data),
        {RespType, Reply, LoopState#loop_state{user_data = UpdatedData}}
    catch
        error:undef ->
            lager:debug("Other handle_call: ~p~n", [Request]),
            {reply, ok, LoopState}
    end.

handle_cast({set_neighbours, NeighbourPids}, LoopState) when is_list(NeighbourPids) ->
    {noreply, LoopState#loop_state{neighbour_pids = NeighbourPids}};
handle_cast({request_data, Request}, LoopState = #loop_state{user_data = UserData, module = Module, neighbour_pids = NeighbourPids, request_data_struct = ReqDataStruct}) ->
    % check if same request is already in pipeline
    case maps:is_key(Request, ReqDataStruct) of
        true ->
            lager:error("Request ~p is already in flight, cannot send duplicate request", [Request]),
            {noreply, LoopState};
        false ->
            {UpdatedRequest, UpdatedUserState} = try
                Module:intercept_request(Request, UserData)
            catch
                error:undef ->
                    lager:debug("Other request_data: ~p", [Request]),
                    {Request, UserData}
            end,
            ReqDatum = request_and_reset_neighbour_data(UpdatedRequest, NeighbourPids),
            {noreply, LoopState#loop_state{user_data = UpdatedUserState, request_data_struct = maps:put(UpdatedRequest, ReqDatum, ReqDataStruct)}}
        end;
handle_cast({request_data, Request, From}, LoopState = #loop_state{user_data = UserData, module = Module}) ->
    {Response, UpdatedUserState} = Module:handle_request_data(Request, UserData),
    respond_data(From, Request, Response, self()),
    {noreply, LoopState#loop_state{user_data = UpdatedUserState}};
handle_cast({response_data, Request, Response, From}, LoopState = #loop_state{user_data = UserData, module = Module, request_data_struct = ReqDataStruct}) ->
    {UpdatedResponseStruct, ReturnUserData} = case maps:find(Request, ReqDataStruct) of
        {ok, ResponseDatum} ->
            {IsComplete, UpdatedResponseDatum} = update_and_check_complete(Response, From, ResponseDatum),
            case IsComplete of
                true ->
                    UpdatedUserData = Module:handle_response_data(get_only_data(UpdatedResponseDatum), UserData),
                    {maps:remove(Request, ReqDataStruct), UpdatedUserData};
                false ->
                    {maps:put(Request, ResponseDatum, ReqDataStruct), UserData}
            end;
        error ->
            lager:warning("Response ~p to request ~p recieved that waas never sent", [Response, Request]),
            {ReqDataStruct, UserData}
    end,
    {noreply, LoopState#loop_state{request_data_struct = UpdatedResponseStruct}};
handle_cast(Msg, LoopState = #loop_state{module = Module, user_data = UserData}) ->
    try
        {RespType, UpdatedData} = Module:handle_cast(Msg, UserData),
        {RespType, LoopState#loop_state{user_data = UpdatedData}}
    catch
        error:undef ->
            lager:debug("Other handle_cast: ~p~n", [Msg]),
            {noreply, LoopState}
    end.

-spec handle_info(any(), loop_state()) -> {noreply, loop_state()}.
handle_info(Info, LoopState = #loop_state{module = Module, user_data = UserData}) ->
    try
        {RespType, UpdatedData} = Module:handle_info(Info, UserData),
        {RespType, LoopState#loop_state{user_data = UpdatedData}}
    catch
        error:undef ->
            lager:debug("Other handle_info: ~p~n", [Info]),
            {noreply, LoopState}
    end.

-spec terminate(any(), loop_state()) -> ok.
terminate(Reason, LoopState = #loop_state{module = Module, user_data = UserData}) ->
    try
        Module:terminate(Reason, UserData)
    catch
        error:undef ->
            lager:debug("Terminating~n")
    end.

-spec code_change(any(), loop_state(), any()) -> {ok, loop_state()}.
code_change(OldVsn, LoopState = #loop_state{module = Module, user_data = UserData}, Extra) ->
    try
        {RespType, UpdatedData} = Module:code_change(OldVsn, UserData, Extra),
        {RespType, LoopState#loop_state{user_data = UpdatedData}}
    catch
        error:undef ->
            lager:debug("Code Changing"),
            {ok, LoopState}
    end.


%%%=============================================================================
%%% Internal
%%%=============================================================================

request_data_internal(NodePid, Request, ReturnAddress) ->
    gen_server:cast(NodePid, {request_data, Request, ReturnAddress}).

respond_data(NodePid, Request, Response, From) ->
    gen_server:cast(NodePid, {response_data, Request, Response, From}).

get_only_data(UpdatedResponseDatum) ->
    {_, Data} = lists:unzip(UpdatedResponseDatum),
    Data.

create_neighbour_objs(Pids) ->
    lists:map(
        fun(Pid) ->
            {Pid, null}
        end,
        Pids).

request_and_reset_neighbour_data(Request, NeighbourPids) ->
    lists:foldl(
        fun(Pid, Acc) ->
            request_data_internal(Pid, Request, self()),
            [{Pid, null} | Acc]
        end,
        [],
        NeighbourPids).

update_and_check_complete(Response, From, ReqDataStruct) ->
    lists:foldl(
        fun(Elem, Acc) -> 
            update_and_check_iter(Elem, Acc, Response, From) 
        end,
        {true, []},
        ReqDataStruct).

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

get_only_data_test() ->
    
    TestCases = [
        {[{null,3}, {null,2}, {null,1}], [3,2,1]},
        {[], []}
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> get_only_data(Input) end).


create_neighbour_objs_test() ->
    
    TestCases = [
        {[1,2,3], [{1,null}, {2,null}, {3,null}]},
        {[], []}
    ],

    test_utilities:action_test_cases(TestCases, fun(Input) -> create_neighbour_objs(Input) end).

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