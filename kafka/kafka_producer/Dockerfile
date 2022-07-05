# Build stage 0
FROM erlang:25-alpine
RUN apk update
RUN apk add git

RUN apk add build-base

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY config kafka_producer/config
# COPY include beer_fest_buddy/include
COPY src kafka_producer/src
COPY rebar.config kafka_producer/rebar.config

# And build the release
WORKDIR kafka_producer
RUN rebar3 release

# Build stage 1
FROM erlang:25-alpine

# # Install the released application
COPY --from=0 /buildroot/kafka_producer/_build/default/rel/kafka_producer /kafka_producer

RUN mkdir /data

CMD "/kafka_producer/bin/kafka_producer" "foreground"