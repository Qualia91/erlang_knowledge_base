#!/usr/bin/env escript
%%! -sname test_erlang_script -mnesia debug verbose

% Force it to compile before running, otherwise it run in interpreted mode
-mode(compile).

% Called when script is called
main([String]) ->
    try
        N = list_to_integer(String),
        F = fac(N),
        io:format("test_erlang_script ~w = ~w\n", [N,F]),
        io:format("Ran in ~s\n", [escript:script_name()])
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: test_erlang_script integer\n"),
    % stop with error code
    halt(69).

fac(0) -> 1;
fac(N) -> N * fac(N-1).