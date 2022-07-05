kafka_test
=====

Example of kafka in erlang using brod

Usage
-----
Start kafka and zookeeper

    docker-compose up -d

Build and start consumer app (cd into kafka_consumer first)

    docker build . -t kafka_consumer --no-cache
    docker run -it --init kafka_consumer

Attach a consumer

    my_consumer:start(<<"my_topic">>, <<"group_id">>).