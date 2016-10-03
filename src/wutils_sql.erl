-module (wutils_sql).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([
         init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

%% happy functions
-export([
         get/1
        ,postgres_range/2
        ,list/0
        ]).

%%% Interface

-spec get(atom()) -> binary().
get(QueryName) ->
    gen_server:call(?MODULE, {get_query, QueryName}).

-spec list() -> [atom()].
list() ->
    gen_server:call(?MODULE, list_queries).

-spec postgres_range(term(), term()) -> string().
postgres_range(Start, End) ->
    lists:flatten([$[, Start, $,, End, $)]).

%%% Gen server

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Config) ->
    SqlSrcApplication = application:get_env(wutils, sql_source_applications, [wutils]),
    Queries = lists:foldl(fun (Application, Acc) ->
                                  {ok, AppQueries} = load_application_sql(Application),
                                  orddict:merge(fun (K, V1, V2) ->
                                                        throw({duplicate_sql, {K, V1, V2}})
                                                end,
                                                AppQueries, Acc)
                          end,
                          orddict:new(),
                          SqlSrcApplication),
    {ok, Queries}.

handle_call({get_query, QueryName}, _From, Queries) ->
    Query = case orddict:find(QueryName, Queries) of
                {ok, Q} ->
                    Q;
                error ->
                    not_found
            end,
    {reply, Query, Queries};
handle_call(list_queries, _From, Queries) ->
    {reply, orddict:to_list(Queries), Queries}.


handle_cast(_Msg, Queries) ->
    {noreply, Queries}.

handle_info(_Info, Queries) -> {noreply, Queries}.
terminate(_Reason, _Queries) -> ok.
code_change(_, _, _) -> {error, unimplemented}.

%%% Private

-spec load_query(string(), list()) -> list().
load_query(Filename, Queries) ->
    lager:debug("loading file: ~p", [Filename]),
    {ok, Q} = file:read_file(Filename),
    [{erlang:list_to_atom(filename:basename(filename:rootname(Filename, ".sql"))), Q} | Queries].

-spec load_application_sql(atom()) -> {ok, orddict:orddict()}.
load_application_sql(Application) ->
    PrivDir = code:lib_dir(Application, priv),
    lager:info("Loading SQL from ~p", [PrivDir]),
    {ok, Filenames} = file:list_dir(PrivDir),
    SqlFilenames = lists:filter(fun (Filename) -> filename:extension(Filename) =:= ".sql" end, Filenames),
    SqlFiles = lists:map(fun (Filename) -> filename:join(PrivDir, Filename) end, SqlFilenames),
    QueryPropList = lists:foldl(fun load_query/2, [], SqlFiles),
    Queries = orddict:from_list(QueryPropList),
    {ok, Queries}.
