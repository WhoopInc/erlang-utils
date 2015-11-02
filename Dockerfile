FROM whoop/erlang-base:18.1
MAINTAINER Nathaniel Waisbrot <waisbrot@whoop.com>

WORKDIR /opt/

COPY . .
RUN rebar3 update && rebar3 update && rebar3 compile

ENTRYPOINT bash
