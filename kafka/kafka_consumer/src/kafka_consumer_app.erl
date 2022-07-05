%%%-------------------------------------------------------------------
%% @doc kafka_consumer public API
%% @end
%%%-------------------------------------------------------------------

-module(kafka_consumer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kafka_consumer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
