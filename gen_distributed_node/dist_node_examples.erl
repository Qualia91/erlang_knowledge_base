%%%-----------------------------------------------------------------------------
%%% @doc
%%% Empty Module built from template.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(dist_node_examples).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    simple_linked_list/0
]).

% c(gen_distributed_node).
% c(messaging_distributed_node).
% c(dist_node_examples).

% dist_node_examples:simple_linked_list().

%%%=============================================================================
%%% API
%%%=============================================================================

simple_linked_list() ->

    {ok, Pid0} = messaging_distributed_node:start_link(0),
    {ok, Pid1} = messaging_distributed_node:start_link(1),
    {ok, Pid2} = messaging_distributed_node:start_link(2),
    {ok, Pid3} = messaging_distributed_node:start_link(3),

    gen_distributed_node:set_neighbours(Pid0, [Pid1]),
    gen_distributed_node:set_neighbours(Pid1, [Pid0, Pid2]),
    gen_distributed_node:set_neighbours(Pid2, [Pid1, Pid3]),
    gen_distributed_node:set_neighbours(Pid3, [Pid2]),

    messaging_distributed_node:request_some_data(Pid0, some_req),
    messaging_distributed_node:request_some_data(Pid1, some_req),
    messaging_distributed_node:request_some_data(Pid2, some_req),
    messaging_distributed_node:request_some_data(Pid3, some_req),

    messaging_distributed_node:send_some_data(Pid0, some_data),
    messaging_distributed_node:send_some_data(Pid1, some_data),
    messaging_distributed_node:send_some_data(Pid2, some_data),
    messaging_distributed_node:send_some_data(Pid3, some_data).