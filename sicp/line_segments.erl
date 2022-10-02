%%%-----------------------------------------------------------------------------
%%% @doc
%%% SICP page 121: Representing line segments.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(line_segments).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    make_point/2,
    x_point/1,
    y_point/1,
    make_segment/2,
    start_segment/1,
    end_segment/1,
    midpoint_segment/1,
    print/1
]).

-record(point, {
    x :: integer(),
    y :: integer()
}).
-type point() :: point.

-record(segment, {
    x :: point(),
    y :: point()
}).

%%%=============================================================================
%%% API
%%%=============================================================================

make_point(X, Y) ->
    #point{x = X, y = Y}.

x_point(#point{x = X}) ->
    X.

y_point(#point{y = Y}) ->
    Y.

make_segment(X, Y) when is_record(X, point), is_record(Y, point) ->
    #segment{x = X, y = Y}.

start_segment(#segment{x = X}) ->
    X.

end_segment(#segment{y = Y}) ->
    Y.

midpoint_segment(Segment) ->
    StartPoint = start_segment(Segment),
    EndPoint = end_segment(Segment),
    HalfX = (x_point(StartPoint) + x_point(EndPoint)) / 2,
    HalfY = (y_point(StartPoint) + y_point(EndPoint)) / 2,
    make_point(HalfX, HalfY).

print(#point{x = X, y = Y}) ->
    io:format("(~p, ~p)~n", [X, Y]);
print(#segment{x = X, y = Y}) ->
    io:format("~p, ~p~n", [X, Y]).    

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

midpoint_test() ->
    A = line_segments:make_point(2,3),
    B = line_segments:make_point(8,6),
    Segment = line_segments:make_segment(A, B),
    Midpoint = line_segments:midpoint_segment(Segment),
    ?assertEqual({point, 5.0, 4.5}, Midpoint).

-endif.