-module(my_consumer).
-include_lib("brod/include/brod.hrl"). 
-behvaiour(brod_group_subscriber_v2).

-export([
  start/2
]).

-export([
  init/2, 
  handle_message/4
]).

%% brod_group_subscriber behaviour callback
init(GroupId, _Arg) ->
  lager:info("GroupId: ~p", [GroupId]),
  {ok, []}.

%% brod_group_subscriber behaviour callback
handle_message(_Topic, _Partition, #kafka_message{value  = Value}, State) ->
	db:set_last_val(Value),
	{ok, State}.

-spec start(binary(), binary()) -> {ok, pid()}.
start(Topic, GroupId) ->
  %% commit offsets to kafka every 5 seconds
  GroupConfig = [
    {offset_commit_policy, commit_to_kafka_v2},
    {offset_commit_interval_seconds, 5}
  ],
  ConsumerConfig = [{begin_offset, earliest}],
  brod:start_link_group_subscriber(brod_consumer_client, GroupId, [Topic],
                                    GroupConfig, ConsumerConfig,
                                    _CallbackModule  = ?MODULE,
                                    _CallbackInitArg = []).