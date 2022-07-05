# Build stage 0
FROM erlang:25-alpine
RUN apk update
RUN apk add git

RUN apk add build-base

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY config kafka_consumer/config
COPY src kafka_consumer/src
COPY rebar.config kafka_consumer/rebar.config

# And build the release
WORKDIR kafka_consumer
RUN rebar3 release

# Build stage 1
FROM erlang:25-alpine

# # Install the released application
COPY --from=0 /buildroot/kafka_consumer/_build/default/rel/kafka_consumer /kafka_consumer

# Expose relevant ports
EXPOSE 8089

CMD "/kafka_consumer/bin/kafka_consumer" "foreground"