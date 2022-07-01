%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author boc_dev
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(interview_question_rover_transform).
-author(nickw).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    create/3,
    apply_action/2,
    check_bounds/3
]).

-define(SERVER, ?MODULE).

-record(transform, {
    x           :: integer(),
    y           :: integer(),
    orientation :: char()
}).
-type transform() :: transform.

%%%=============================================================================
%%% API
%%%=============================================================================

create(X, Y, Ori) ->
    #transform{
        x           = X, 
        y           = Y, 
        orientation = Ori
    }.

apply_action(Transform = #transform{orientation = lost}, $F) when is_record(Transform, transform)->
    Transform;
apply_action(Transform, $F) when is_record(Transform, transform)->
    move_foward(Transform);
apply_action(Transform, $L) when is_record(Transform, transform)->
    rotate_anticlockwise(Transform);
apply_action(Transform, $R) when is_record(Transform, transform)->
    rotate_clockwise(Transform).

check_bounds(MaxX, MaxY, #transform{x = X, y = Y}) when X >= 0, Y >= 0, X =< MaxX, Y =< MaxY ->
    in_bounds;
check_bounds(_, _, _) ->
    out_of_bound.

%%%===================================================================
%%% Internal functions
%%%===================================================================

move_foward(#transform{x = X, y = Y, orientation = $N}) ->
    #transform{x = X, y = Y + 1, orientation = $N};
move_foward(#transform{x = X, y = Y, orientation = $S}) ->
    #transform{x = X, y = Y - 1, orientation = $S};
move_foward(#transform{x = X, y = Y, orientation = $E}) ->
    #transform{x = X + 1, y = Y, orientation = $E};
move_foward(#transform{x = X, y = Y, orientation = $W}) ->
    #transform{x = X - 1, y = Y, orientation = $W}.

rotate_anticlockwise(Transform = #transform{orientation = $N}) ->
    Transform#transform{orientation = $W};
rotate_anticlockwise(Transform = #transform{orientation = $W}) ->
    Transform#transform{orientation = $S};
rotate_anticlockwise(Transform = #transform{orientation = $S}) ->
    Transform#transform{orientation = $E};
rotate_anticlockwise(Transform = #transform{orientation = $E}) ->
    Transform#transform{orientation = $N}.

rotate_clockwise(Transform = #transform{orientation = $N}) ->
    Transform#transform{orientation = $E};
rotate_clockwise(Transform = #transform{orientation = $E}) ->
    Transform#transform{orientation = $S};
rotate_clockwise(Transform = #transform{orientation = $S}) ->
    Transform#transform{orientation = $W};
rotate_clockwise(Transform = #transform{orientation = $W}) ->
    Transform#transform{orientation = $N}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

example_test() ->
    ?assertEqual(true, true).

-endif.
