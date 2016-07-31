%%% @copyright (C) 2015, WHOOP
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2015 by Nathaniel Waisbrot <waisbrot@whoop.com>
-module(wutils_redis).
-author('waisbrot@whoop.com').

% API
-export([
         open/0, open/1
        ,close/1, close/2
        ,run/1, run_many/1
        ]).

% for episcina
-export([
         ep_open/0
        ,ep_close/1
        ]).

-type redis_client() :: {redis_client, pid()}.

-spec open() -> redis_client() | {error,term()}.
-spec open(atom()) -> redis_client() | {error,term()}.
open() ->
    open(redis).
open(Pool) ->
    case episcina:get_connection(Pool) of
        {ok, Pid} ->
            Pid;
        {error, Reason} ->
            {error, Reason}
    end.

-spec close(redis_client()) -> ok.
-spec close(atom(), redis_client()) -> ok.
close(Pid) ->
    close(redis, Pid).
close(Pool, Pid) ->
    episcina:return_connection(Pool, Pid).

-spec run(list()) -> {ok, [any()]}.
run(Query) ->
    RD = open(redis),
    Result = eredis:q(RD, Query),
    close(redis, RD),
    Result.
-spec run_many(list()) -> {ok, [any()]}.
run_many(Query) ->
    RD = open(redis),
    Result = eredis:qp(RD, Query),
    close(redis, RD),
    Result.

-spec ep_open() -> {ok, pid()}.
ep_open() ->
    Args = [{host, wutils_config:get({"RDHOST", rdhost, string, "localhost"})}
        ,{port, wutils_config:get({"RDPORT", rdport, integer, 6379})}
        ,{password, wutils_config:get({"RDPASSWORD", rdpassword, string, false})}],
    {ok, Pid} = eredis:start_link(Args),
    {ok, Pid}.

-spec ep_close(pid()) -> ok.
ep_close(Pid) ->
    eredis:stop(Pid).
