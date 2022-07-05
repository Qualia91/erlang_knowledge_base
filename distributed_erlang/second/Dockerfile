# Build stage 0
FROM erlang:25-alpine
RUN apk update
RUN apk add git

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY src second/src
COPY rebar.config second/rebar.config
COPY config/ second/config

# And build the release
WORKDIR second
RUN rebar3 release

# Build stage 1
FROM erlang:25-alpine

# # Install the released application
COPY --from=0 /buildroot/second/_build/default/rel/second /second

CMD /second/bin/second foreground