-module(kcl_app).
-behaviour(application).

-export([stop/1, start/2]).

%%% APPLICATION
-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_, _) ->

    ok.

-spec stop(term()) -> ok.
stop(_) ->
    ok.

%%% Private
get_
