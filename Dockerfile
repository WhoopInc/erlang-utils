FROM docker.whoop.com/whoop/aws-erlang:17.5
MAINTAINER Nathaniel Waisbrot <waisbrot@whoop.com>

ADD . .
RUN rebar3 update && rebar3 compile

ENTRYPOINT bash
