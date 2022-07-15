%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(test_utilities).
-author(nickw).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    action_test_cases/2
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

action_test_cases(TestCases, Function) ->
    lists:foreach(
        fun({Input, ExpectedOutput}) ->
            ?assertEqual(ExpectedOutput, Function(Input))
        end,
        TestCases
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

action_test_cases_test() ->

    TestCases = [
        {1, 1}
    ],

    action_test_cases(TestCases, fun(Input) -> Input end).

-endif.
