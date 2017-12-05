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
        ,run_backup/1, run_backup/2
        ,with_connection/1
        ,with_connection/2
        ,with_backup_connection/1
        ,with_backup_connection/2
        ,start_pools/1, start_pools/0
        ,query/2, query/3
        ]).

%%% for episcina
-export([
         ep_open/0
        ,ep_backup_open/0
        ,ep_close/1
        ,ep_backup_close/1
        ]).

-type connection() :: pgsql_connection:pgsql_connection().

-export_type([connection/0]).

start_pools() ->
    start_pools(wutils).
start_pools(Application) ->
    case application:get_env(Application, pools) of
        {ok, Pools} ->
            PoolResults = lists:zip(Pools, episcina:start_pools(Pools)),
            lists:foreach(fun ({{PoolName, _}, {error, Reason}}) ->
                                  error({"unable to start pool: "++atom_to_list(PoolName), Reason});
                              (_) ->
                                  ok
                          end, PoolResults),
            ok;
        _ ->
            ok
    end.

-spec open() -> connection() | {error,timeout}.
-spec open(atom()) -> connection() | {error,timeout}.
open() ->
    open(primary).
open(Pool) ->
    case episcina:get_connection(Pool) of
        {ok, Pid} ->
            {pgsql_connection, Pid};
        {error, timeout} ->
            {error, timeout}
    end.
-spec close(connection()) -> ok.
-spec close(atom(), connection()) -> ok.
close(Pid) ->
    close(primary, Pid).
close(Pool, {pgsql_connection, Pid}) ->
    episcina:return_connection(Pool, Pid).

-spec with_connection(fun((connection()) -> Result)) -> Result.
with_connection(Fun) ->
    with_connection(Fun, primary).

-spec with_backup_connection(fun((connection()) -> Result)) -> Result.
with_backup_connection(Fun) ->
    with_connection(Fun, backup).

-spec with_connection(fun((connection()) -> Result), atom()) -> Result.
with_connection(Fun, Pool) ->
    case open(Pool) of
      {error, timeout} ->
        error({error, get_connection_timeout});
      PG ->
        Result = Fun(PG),
        close(Pool, PG),
        Result
    end.

-spec run(string()|binary()) -> pgsql_connection:result_tuple().
-spec run(string()|binary(), list()) -> pgsql_connection:result_tuple().
run(Query) ->
    with_connection(fun (PG) ->
                            query(Query, PG)
                    end).
run(Query, Bindings) ->
    with_connection(fun (PG) ->
                            query(Query, Bindings, PG)
                    end).

-spec run_backup(string()|binary()) -> pgsql_connection:result_tuple().
-spec run_backup(string()|binary(), list()) -> pgsql_connection:result_tuple().
run_backup(Query) ->
    with_backup_connection(fun (PG) ->
                            query(Query, PG)
                    end).
run_backup(Query, Bindings) ->
    with_backup_connection(fun (PG) ->
                            query(Query, Bindings, PG)
                    end).

query(Query, PG) ->
    %% lager:debug("connection=~p query=~p", [Query, PG]),
    try pgsql_connection:simple_query(Query, PG) of
        V ->
            V
    catch
        exit:connection_timeout ->
            close(PG),
            error({pgsql, connection_timeout, PG});
        Type:Reason ->
            close(PG),
            error({pgsql, Type, Reason, PG})
    end.

query(Query, Args, PG) ->
    %% lager:debug("connection=~p, query=~p", [Query, PG]),
    try pgsql_connection:extended_query(Query, Args, PG) of
        V ->
            V
    catch
        exit:connection_timeout ->
            close(PG),
            error({pgsql, connection_timeout, PG});
        Type:Reason ->
            close(PG),
            erlang:display(erlang:get_stacktrace()),
            error({pgsql, Type, Reason, PG})
    end.

-define(APPLICATION, wutils).

-spec ep_open() -> {ok, pid()}.
ep_open() ->
    {pgsql_connection, Pid} = pgsql_connection:open(
        [
         {host, stillir:get_config(?APPLICATION, pg_host)}
        ,{database, stillir:get_config(?APPLICATION, pg_database)}
        ,{port, stillir:get_config(?APPLICATION, pg_port)}
        ,{user, stillir:get_config(?APPLICATION, pg_user)}
        ,{password, stillir:get_config(?APPLICATION, pg_password)}
        ]),
    {ok, Pid}.

-spec ep_close(pid()) -> ok.
ep_close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).

%% backup conn entry point %%

-spec ep_backup_open() -> {ok, pid()}.
ep_backup_open() ->
    {pgsql_connection, Pid} = pgsql_connection:open(
        [
         {host, stillir:get_config(?APPLICATION, bu_host)}
        ,{database, stillir:get_config(?APPLICATION, bu_database)}
        ,{port, stillir:get_config(?APPLICATION, bu_port)}
        ,{user, stillir:get_config(?APPLICATION, bu_user)}
        ,{password, stillir:get_config(?APPLICATION, bu_password)}
        ]),
    {ok, Pid}.

-spec ep_backup_close(pid()) -> ok.
ep_backup_close(Pid) ->
    pgsql_connection:close({pgsql_connection, Pid}).
