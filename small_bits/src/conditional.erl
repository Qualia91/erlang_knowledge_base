%%%-----------------------------------------------------------------------------
%%% @doc
%%% Sequential patterns in erlang
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(conditional).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    conditional/1
]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% Replaced by maybe in OPT 25
conditional([]) ->
    ok;
conditional([{Cond, Fun} | _]) when Cond == else, Cond == true ->
    Fun();
conditional([{false, _} | Rest]) ->
    conditional(Rest);
conditional([{Predicate, Fun} | Rest]) ->
    case Predicate() of
        else  -> Fun();
        true  -> Fun();
        false -> conditional(Rest)
    end.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cond_test(Var1, Var2) ->
    conditional([
        {fun() -> 1 == 2 end, fun() -> 0 end},
        {else, fun() -> 2 end}
    ]).

-endif.