%%%-----------------------------------------------------------------------------
%%% @doc
%%% Empty Module built from template.
%%% Aim:
%%% parse: test(_, 1, _). which would create a function of arity 2 that returns
%%% test/3 with 1 as middle variable.
%%% @author boc_dev
%%% @end
%%%-----------------------------------------------------------------------------

-module(test).
-author("boc_dev").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    main/0,
    test/3
]).

-define(SERVER, ?MODULE).

main() ->
    _ = test(1).

test(A) ->
    fun (B, C) -> A + B + C + 1 end.
test(A, B, C) ->
    A + B + C + 1.