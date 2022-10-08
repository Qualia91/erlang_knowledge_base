%%%-----------------------------------------------------------------------------
%%% @doc
%%% A way to do class like objects in erlang.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(classes).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    my_class/3,
    run/2,
    run/3
]).

-record(my_class, {
    member_variables,
    public_functions
}).
-record(my_class_vars, {
    var_1,
    var_2,
    var_3
}).

%%%=============================================================================
%%% API
%%%=============================================================================

my_class(Var1, Var2, Var3) ->
    MV = #my_class_vars{var_1 = Var1, var_2 = Var2, var_3 = Var3},
    #my_class{
        member_variables = MV,
        public_functions = #{
            do_something_1 => fun do_something_1/2,
            get_something_1 => fun get_something_1/1
        }
    }.

run(MyClass = #my_class{public_functions = PF, member_variables = MV}, FunctionName) ->
    FuncRet = case maps:find(FunctionName, PF) of
        {ok, Function} ->
            Function(MV);
        error ->
            io:format("~p~n", [<<"Function not found">>]),
            MV
    end,
    case FuncRet of
        {Return, UpdatedMV} -> {Return, MyClass#my_class{member_variables = UpdatedMV}};
        UpdatedMV -> MyClass#my_class{member_variables = UpdatedMV}
    end.
run(MyClass = #my_class{public_functions = PF, member_variables = MV}, FunctionName, Inputs) ->
    FuncRet = case maps:find(FunctionName, PF) of
        {ok, Function} ->
            Function(MV, Inputs);
        error ->
            io:format("~p~n", [<<"Function not found">>]),
            MV
    end,
    case FuncRet of
        {Return, UpdatedMV} -> {Return, MyClass#my_class{member_variables = UpdatedMV}};
        UpdatedMV -> MyClass#my_class{member_variables = UpdatedMV}
    end.
            

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_something_1(MV = #my_class_vars{var_1 = Var1}, {In1, In2}) ->
    MV#my_class_vars{var_1 = Var1 + In1 + In2}.

get_something_1(MyClass) ->
    {hello, MyClass}.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

my_class_test() ->

    MyClass = classes:my_class(5,6,7),
    MyClass1 = classes:run(MyClass, do_something, {1, 2}),
    MyClass2 = classes:run(MyClass1, do_something_1, {1, 2}),
    {Return, MyClass3} = classes:run(MyClass2, get_something_1),

    ?assertEqual(hello, Return),
    ?assertEqual(8, MyClass3#my_class_vars.var_1),
    ?assertEqual(6, MyClass3#my_class_vars.var_2),
    ?assertEqual(7, MyClass3#my_class_vars.var_3).

-endif.