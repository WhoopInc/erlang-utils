%%% @copyright (C) 2015, WHOOP
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2015 by Nathaniel Waisbrot <waisbrot@whoop.com>
-module(wutils_db).
-author('waisbrot@whoop.com').

% API
-export([
	 open/0, close/1,
	 open/1, close/2
	]).

% for episcina
-export([ep_open/0, ep_close/1]).

open() ->
    open(primary).
open(Pool) ->
    case episcina:get_connection(Pool) of
	{ok, Pid} ->
	    {pgsql_connection, Pid};
	{error, timeout} ->
	    {error, timeout}
    end.
close(Pid) ->
    close(primary, Pid).
close(Pool, {pgsql_connection, Pid}) ->
    episcina:return_connection(Pool, Pid).

getenv(Var) ->
    getenv(Var, false, []).
getenv(Var, Default, []) ->
    case os:getenv(Var) of
        false ->
            Default;
        Val ->
            Val
    end;
getenv(Var, Default, [integer|Rest]) ->
    Val = getenv(Var, Default, Rest),
    list_to_integer(Val).


ep_open() ->
    {pgsql_connection, Pid} = pgsql_connection:open(
				[{host, wutils_config:get({"PGHOST", pghost, string, "localhost"})}
        ,{database, wutils_config:get({"PGDATABASE", pgdatabase, string, "postgres"})}
        ,{port, wutils_config:get({"PGPORT", pgport, integer, 5432})}
        ,{user, wutils_config:get({"PGUSER", pguser, string, "postgres"})}
        ,{password, wutils_config:get({"PGPASSWORD", pgpassword, string, false})}]),
    {ok, Pid}.

ep_close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).
