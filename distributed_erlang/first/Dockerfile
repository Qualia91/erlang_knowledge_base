# Build stage 0
FROM erlang:25-alpine
RUN apk update
RUN apk add git

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY src first/src
COPY rebar.config first/rebar.config
COPY config/ first/config

# And build the release
WORKDIR first
RUN rebar3 release

# Build stage 1
FROM erlang:25-alpine

# # Install the released application
COPY --from=0 /buildroot/first/_build/default/rel/first /first

# Expose relevant ports
EXPOSE 8080

CMD /first/bin/first foreground