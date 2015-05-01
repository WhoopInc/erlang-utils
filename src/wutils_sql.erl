-module (wutils_sql).
-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% happy functions
-export([get/1, postgres_range/2]).
-include("wutils.hrl").


%  88                                                   ad88                                      %
%  ""                 ,d                               d8"                                        %
%                     88                               88                                         %
%  88  8b,dPPYba,   MM88MMM   ,adPPYba,  8b,dPPYba,  MM88MMM  ,adPPYYba,   ,adPPYba,   ,adPPYba,  %
%  88  88P'   `"8a    88     a8P_____88  88P'   "Y8    88     ""     `Y8  a8"     ""  a8P_____88  %
%  88  88       88    88     8PP"""""""  88            88     ,adPPPPP88  8b          8PP"""""""  %
%  88  88       88    88,    "8b,   ,aa  88            88     88,    ,88  "8a,   ,aa  "8b,   ,aa  %
%  88  88       88    "Y888   `"Ybbd8"'  88            88     `"8bbdP"Y8   `"Ybbd8"'   `"Ybbd8"'  %

get(QueryName) ->
  gen_server:call(?MODULE, {get_query, QueryName}).

postgres_range(Start, End) ->
  lists:flatten([$[, Start, $,, End, $)]).


%   ,adPPYb,d8   ,adPPYba,  8b,dPPYba,                 ,adPPYba,   ,adPPYba,  8b,dPPYba,  8b       d8   ,adPPYba,  8b,dPPYba,  %
%  a8"    `Y88  a8P_____88  88P'   `"8a                I8[    ""  a8P_____88  88P'   "Y8  `8b     d8'  a8P_____88  88P'   "Y8  %
%  8b       88  8PP"""""""  88       88                 `"Y8ba,   8PP"""""""  88           `8b   d8'   8PP"""""""  88          %
%  "8a,   ,d88  "8b,   ,aa  88       88                aa    ]8I  "8b,   ,aa  88            `8b,d8'    "8b,   ,aa  88          %
%   `"YbbdP"Y8   `"Ybbd8"'  88       88                `"YbbdP"'   `"Ybbd8"'  88              "8"       `"Ybbd8"'  88          %
%   aa,    ,88                                                                                                                 %
%    "Y8bbdP"                            888888888888                                                                          %

start_link(_Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Config) ->
    SqlSrcApplication = application:get_env(sql_source_application),
    {ok, Queries} = load_application_sql(SqlSrcApplication),
    {ok, Queries}.

handle_call({get_query, QueryName}, _From, Queries) ->
  Query = case orddict:find(QueryName, Queries) of
    {ok, Q} ->
      Q;
    error ->
      error
  end,
  % get query
  {reply, Query, Queries}.

handle_cast(_Msg, Queries) ->
  % ?TRACE("unknown cast in checker! ~p~n~p~n", [Msg, Queries]),
  {noreply, Queries}.

handle_info(_Info, Queries) -> {noreply, Queries}.
terminate(_Reason, _Queries) -> ok.
code_change(_, _, _) -> unimplemented.

%                                    88                           %
%                                    ""                           %
%                                                                 %
% 88,dPYba,,adPYba,   ,adPPYYba,     88   ,adPPYba,   8b,dPPYba,  %
% 88P'   "88"    "8a  ""     `Y8     88  a8"     "8a  88P'   "Y8  %
% 88      88      88  ,adPPPPP88     88  8b       d8  88          %
% 88      88      88  88,    ,88     88  "8a,   ,a8"  88          %
% 88      88      88  `"8bbdP"Y8     88   `"YbbdP"'   88          %
%                                   ,88                           %
%                                 888P"                           %


load_query(Filename, Queries) ->
    ?DEBUG("loading file: ~p", [Filename]),
    {ok, Q} = file:read_file(Filename),
    [{filename:basename(filename:rootname(Filename, ".sql")),Q}|Queries].

load_application_sql(Application) ->
    PrivDir = code:lib_dir(Application, priv),
    ?DEBUG("Loading SQL from ~p", [PrivDir]),
    {ok, Filenames} = file:list_dir(PrivDir),
    SqlFilenames = lists:filter(fun (Filename) -> filename:extension(Filename) =:= ".sql" end, Filenames),
    SqlFiles = lists:map(fun (Filename) -> filename:join(PrivDir, Filename) end, SqlFilenames),
    QueryPropList = lists:foldl(fun load_query/2, [], SqlFiles),
    Queries = orddict:from_list(QueryPropList),
    {ok, Queries}.
