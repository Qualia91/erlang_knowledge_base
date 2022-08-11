#!/opt/homebrew/opt/erlang@24/bin/escript
%% -*- erlang -*-

%%%=============================================================================
%%% EScript Functions
%%%=============================================================================

main([StringMap]) ->
    try
        Desc = describe_map(check_params_by_comma(StringMap)),
        io:format("~p\n", [Desc])
    catch
        Error:Reason ->
            io:format("Error: ~p, Reason: ~p\n", [Error, Reason]),
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: describe_map map\n"),
    halt(1).


%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

check_params_by_comma(Params) ->
    % Tokenize the incoming string
    {ok, Tokens, _} = erl_scan:string(Params ++ "."),

    % Convert it to abstract syntax tree (AST)
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),

    % Convert AST to term
    {value, Term, _} = erl_eval:exprs(Exprs, []),
    Term.

%%------------------------------------------------------------------------------
%% @doc
%% Starts the recursive map description creation function.
%% @end
%%------------------------------------------------------------------------------
describe_map(Map) when is_map(Map) ->
    create_key_structure(maps:to_list(Map), []).

%%------------------------------------------------------------------------------
%% @doc
%% Recursively goes through map to create a description structure in a tree form, 
%% made of lists of tuples which show field name and type.
%% @end
%%------------------------------------------------------------------------------
create_key_structure([], Acc) ->
    Acc;
create_key_structure([{Key, Value} | Tl], Acc) when is_map(Value) ->
    create_key_structure(Tl, [{Key, {'__is_map__', describe_map(Value)}} | Acc]);
create_key_structure([{Key, Value} | Tl], Acc) ->
    create_key_structure(Tl, [{Key, get_type(Value)} | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%% Way of assigning a description of a type in erlang.
%% @end
%%------------------------------------------------------------------------------
get_type(Value) when is_atom(Value) ->
    '__is_atom__';
get_type(Value) when is_integer(Value) ->
    '__is_integer__';
get_type(Value) when is_number(Value) ->
    '__is_number__';
get_type(Value) when is_binary(Value) ->
    '__is_binary__';
get_type(Value) when is_boolean(Value) ->
    '__is_boolean__';
get_type(Value) when is_list(Value) ->
    '__is_list__'.