%%%-------------------------------------------------------------------
%% @doc wutils top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wutils_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-define(CHILD(ChildModule), {ChildModule, {ChildModule, start_link, []}, permanent, brutal_kill, worker, [ChildModule]}).
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1},
          [
           ?CHILD(wutils_sql)
          ]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
