%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(applicative).
-author(nickw).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([]).

-define(SERVER, ?MODULE).

% -type functor(Type) :: maybe(any()) | either(Type) | list(Type) | Type.
% -type applicative(Type) :: fun(() -> functor(Type)).

%%%=============================================================================
%%% API
%%%=============================================================================

apply(nothing, _) ->
    nothing;
apply(_, nothing) ->
    nothing;
apply(ApplicativeFn, Val) ->
    ApplicativeFn(Val).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

string_concat(StrA, StrB) ->
    StrA ++ " " ++ StrB.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_applicative_test() ->

    apply(fun string_concat/2, ),

    io:format("HERE:").

    % Cases = [
    %     %{nothing, nothing, fun maybe_int_to_string/1}
    % ],
    
    % run_assert(Cases).

%%%=============================================================================
%%% Internal test functions
%%%=============================================================================

run_assert([]) ->
    ok;
run_assert([{Input, Expected, Function} | Tl]) ->
    ?assertEqual(Expected, Function(Input)),
    run_assert(Tl).

-endif.
