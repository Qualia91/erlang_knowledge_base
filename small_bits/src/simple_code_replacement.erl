%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(simple_code_replacement).
-author(boc_dev).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    loop/0
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

loop() ->
    receive
        _ ->
            io:format("This is old code"),
            simple_code_replacement:loop()
    end.

% c(simple_code_replacement).
% Pid = spawn(fun simple_code_replacement:loop/0).
% Pid ! hello.

% When reloaded once, code is updated and loop carries on

% loop() ->
%     receive
%         _ ->
%             io:format("This is new code"),
%             loop()
%     end.

% c(simple_code_replacement).
% Pid ! hello. % Will return old code
% Pid ! hello. % Will return new code

% If loaded twice, the module is nuked and loop isnt running anymore :(

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
