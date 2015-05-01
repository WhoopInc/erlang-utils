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

ep_open() ->
    {pgsql_connection, Pid} = pgsql_connection:open(
				[{host, os:getenv("PGHOST")},
				 {database, os:getenv("PGDATABASE")},
				 {user, os:getenv("PGUSER")},
				 {password, os:getenv("PGPASSWORD")}]),
    {ok, Pid}.

ep_close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).
