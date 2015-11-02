%%%
%%% Supervisor provided so that
-module(wutils_metric_sup).
-behaviour(supervisor).

-export([init/1]).
-export([
         start/1, start/2, start/3
        ]).

%%% API
start(ApplicationName) ->
    start(ApplicationName, #{}).

start(ApplicationName, Tags) ->
    DefaultSupFlags = #{
      strategy => one_for_one,
      intensity => 1,
      period => 5
     },
    start(ApplicationName, Tags, DefaultSupFlags).

start(ApplicationName, Tags, SupFlags) ->
    supervisor:start_link(?MODULE, [ApplicationName, Tags, SupFlags]).

%%% Supervisor
init([ApplicationName, Tags, SupFlags]) ->
    Children = [#{
                   id => wutils_metric,
                   start => {wutils_metric, start, [ApplicationName, Tags]},
                   shutdown => brutal_kill
                 }],
    {ok, SupFlags, Children}.
