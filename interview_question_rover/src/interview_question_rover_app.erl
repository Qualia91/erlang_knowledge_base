%%%-------------------------------------------------------------------
%% @doc interview_question_rover public API
%% @end
%%%-------------------------------------------------------------------

-module(interview_question_rover_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    interview_question_rover_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
