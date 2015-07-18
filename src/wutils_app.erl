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

%%====================================================================
%% API
%%====================================================================
-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    wutils_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
