%%%-----------------------------------------------------------------------------
%%% @doc
%%% This module contains lots of functions to help you write code more like Haskell.
%%%
%%% @author nickw
%%% @copyright MIT
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(functional_programming_toolbox).
-author(nickw).

-include_lib("functional_programming_toolbox.hrl").

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

% API
-export(['?>'/2, chain/2, do_something/1, now_do_thing/1, and_then_do_this/1, print_maybe/1]).

% Example Usages
-export([examples/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Chaining Operator.
%% This function takes in a function and a maybe monad. If the maybe is a just,
%% then it runs the function. If not, it just returns the maybe. This can be used
%% to chain functions together with the option of an early exit from the chain.
%% @end
%%-----------------------------------------------------------------------------
-spec '?>'(fun((any()) -> maybe(any())), maybe(any())) -> maybe(any()).
'?>'(Function, #just{val = Val}) ->
    Function(Val);
'?>'(_Function, Maybe = #nothing{}) ->
    Maybe.

%%-----------------------------------------------------------------------------
%% @doc Chaining Function.
%% This function uses the chain operator with the input list of functions to
%% create a chain of functions with an early exit using the maube monad.
%% @end
%%-----------------------------------------------------------------------------
-spec chain(list(fun((any()) -> maybe(any()))), any()) -> maybe(any()).
chain([], Input) ->
    #just{val = Input};
chain([Func | Funcs], Input) ->
    lists:foldl(
        fun(IterFunc, Acc) ->
            '?>'(IterFunc, Acc)
        end,
        Func(Input),
        Funcs
    ).

%%%===================================================================
%%% Example Usages
%%%===================================================================

examples() ->

    %%% '?>' Usage
    
    % First create functions that take in a value and returns a maybe
    Func1 = fun(InputVal) -> 
        case InputVal of
            "Hello" -> #just{val = InputVal};
            _    -> #nothing{}
        end
    end,
    Func2 = fun(InputVal) -> 
        case InputVal of
            "NotHello" -> #just{val = InputVal};
            _    -> #nothing{}  
        end    
    end,

    % Now we can chain this function
    % The first one will run both functions and a fail on the Func2
    % The second one wont run Func1, as Func2 has failed already
    % The third one will run all the functions and return a Just
    print_maybe('?>'(Func2, Func1("Hello"))),
    print_maybe('?>'(Func1, Func2("Hello"))),
    print_maybe('?>'(Func1, Func1("Hello"))),
    

    ReturnMaybe = chain([
        do_something/1,
        now_do_thing/1,
        and_then_do_this/1
    ],
    "On This Data"),
    

    Data = "On This Data",
    case do_something(Data) of
        {error, Reason1} -> Reason1;
        Result1 -> case now_do_thing(Result1) of
            {error, Reason2} -> Reason2;
            Result2 -> case and_then_do_this(Result2) of
                {error, Reason3} -> Reason3;
                Result3 -> Result3
            end
        end
    end.
    
do_something(Str) ->
    #just{val = Str}.

now_do_thing(Str) ->
    #just{val = Str}.

and_then_do_this(Str) ->
    #just{val = Str}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

print_maybe(#just{val = Val}) ->
    io:format("Just:~p~n", [Val]);
print_maybe(#nothing{}) ->
    io:format("Nothing~n").