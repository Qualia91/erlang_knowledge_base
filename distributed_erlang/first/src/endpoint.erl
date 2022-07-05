-module(endpoint).

-export([init/2]).
-export([content_types_provided/2]).
-export([example/2]).
-export([allowed_methods/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, example},
		{<<"application/json">>, example},
		{<<"text/plain">>, example}
	], Req, State}.

example(Req, State) ->

	Server = second@second_host,
	monitor_node(Server, true),
	Resp = gen_server:call({server, Server}, req),
	monitor_node(Server, false),

	{Resp, Req, State}.
