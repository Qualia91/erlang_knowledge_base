%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application module.
%%% @author boc_dev
%%% @copyright Apache 2.0
%%% @version 0.1.0
%%% @date 2022-07-13
%%% @end
%%%-----------------------------------------------------------------------------

-module(distributed_systems_course_app).
-author(boc_dev).

-behaviour(application).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([
	start/2, 
	stop/1
]).

%%%=============================================================================
%%% API
%%%=============================================================================

start(_StartType, _StartArgs) ->
    distributed_systems_course_sup:start_link().

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal
%%%=============================================================================
