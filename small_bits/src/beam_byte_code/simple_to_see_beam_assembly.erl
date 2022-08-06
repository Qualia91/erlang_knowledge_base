%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @end
%%%-----------------------------------------------------------------------------

-module(simple_to_see_beam_assembly).
-author("boc_dev").

% run:
% erlc -S simple_to_see_beam_assembly.erl
% to see byte coode in human readable format (.s file)

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([some_func/1]).

some_func(A) ->
    A + 1.