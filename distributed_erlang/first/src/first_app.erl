%%%-------------------------------------------------------------------
%% @doc first public API
%% @end
%%%-------------------------------------------------------------------

-module(first_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    first_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
