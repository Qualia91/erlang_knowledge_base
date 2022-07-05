%%%-------------------------------------------------------------------
%% @doc kafka_producer public API
%% @end
%%%-------------------------------------------------------------------

-module(kafka_producer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kafka_producer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
