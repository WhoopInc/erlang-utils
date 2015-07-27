FROM docker.whoop.com/whoop/aws-erlang:17.5
MAINTAINER Nathaniel Waisbrot <waisbrot@whoop.com>

RUN curl -L https://s3.amazonaws.com/rebar3/rebar3 -o /opt/erlang/bin/rebar3 && chmod a+x /opt/erlang/bin/rebar3
WORKDIR /opt/

ADD . .
RUN rebar3 update && rebar3 update && rebar3 compile

ENTRYPOINT bash
