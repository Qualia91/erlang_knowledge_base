%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @author nickw
%%% @copyright <COPY_WRITE>
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(my_publisher).
-author(nickw).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% External API
-export([
    start/2,
    publish/4
]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start(TopicName, ProducerConfig) ->
    brod:start_producer(brod_producer_client, TopicName, ProducerConfig).

publish(Topic, Partition, Key, Message) ->
    brod:produce_sync(brod_producer_client, Topic, Partition, Key, Message).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
