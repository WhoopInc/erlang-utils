%%%-------------------------------------------------------------------
%% @doc wutils public API
%% @end
%%%-------------------------------------------------------------------

-module(wutils_app).

-behaviour(application).

%% Application callbacks
-export([
         start/2
        ,stop/1
        ]).
-define(APPLICATION, wutils).
%%====================================================================
%% API
%%====================================================================
-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    Config = [
              {pg_host, "PGHOST", [required]}
             ,{pg_database, "PGDATABASE", [required]}
             ,{pg_port, "PGPORT", [{transform, integer}, {default, 5432}]}
             ,{pg_user, "PGUSER", [required]}
             ,{pg_password, "PGPASSWORD", [required]}

             ,{bu_host, "BUHOST", [required]}
             ,{bu_database, "BUDATABASE", [required]}
             ,{bu_port, "BUPORT", [{transform, integer}, {default, 5432}]}
             ,{bu_user, "BUUSER", [required]}
             ,{bu_password, "BUPASSWORD", [required]}
             ],
    ok = stillir:set_config(?APPLICATION, Config),
    ok = wutils_db:start_pools(),
    wutils_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
