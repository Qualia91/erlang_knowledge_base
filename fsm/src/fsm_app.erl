%%%-------------------------------------------------------------------
%% @doc fsm public API
%% @end
%%%-------------------------------------------------------------------

-module(fsm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fsm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
