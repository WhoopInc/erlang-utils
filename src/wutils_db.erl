%%% @copyright (C) 2015, WHOOP
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2015 by Nathaniel Waisbrot <waisbrot@whoop.com>
-module(wutils_db).
-author('waisbrot@whoop.com').

% API
-export([
         open/0, open/1
        ,close/1, close/2
        ,run/1, run/2
        ]).

% for episcina
-export([
         ep_open/0
        ,ep_close/1
        ]).

-spec open() -> pgsql_connection:pgsql_connection() | {error,timeout}.
-spec open(atom()) -> pgsql_connection:pgsql_connection() | {error,timeout}.
open() ->
    open(primary).
open(Pool) ->
    case episcina:get_connection(Pool) of
        {ok, Pid} ->
            {pgsql_connection, Pid};
        {error, timeout} ->
            {error, timeout}
    end.
-spec close(pgsql_connection:pgsql_connection()) -> ok.
-spec close(atom(), pgsql_connection:pgsql_connection()) -> ok.
close(Pid) ->
    close(primary, Pid).
close(Pool, {pgsql_connection, Pid}) ->
    episcina:return_connection(Pool, Pid).

-spec run(string()|binary()) -> pgsql_connection:result_tuple().
-spec run(string()|binary(), list()) -> pgsql_connection:result_tuple().
run(Query) ->
    PG = open(),
    Result = PG:simple_query(Query),
    close(PG),
    Result.
run(Query, Bindings) ->
    PG = open(),
    Result = PG:extended_query(Query, Bindings),
    close(PG),
    Result.

-spec ep_open() -> {ok, pid()}.
ep_open() ->
    {pgsql_connection, Pid} = pgsql_connection:open(
				[{host, wutils_config:get({"PGHOST", pghost, string, "localhost"})}
        ,{database, wutils_config:get({"PGDATABASE", pgdatabase, string, "postgres"})}
        ,{port, wutils_config:get({"PGPORT", pgport, integer, 5432})}
        ,{user, wutils_config:get({"PGUSER", pguser, string, "postgres"})}
        ,{password, wutils_config:get({"PGPASSWORD", pgpassword, string, false})}]),
    {ok, Pid}.

-spec ep_close(pid()) -> ok.
ep_close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).
