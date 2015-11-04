-module(wutils_metric).
-behaviour(gen_server).

-export([
         init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).
-export([
         start/1, start/2
        ,write/2, write/3
        ]).

%%% Types
-type application_name() :: nonempty_string().
-type key() :: atom().
-type value() :: atom() | string() | number().
-type tags() :: #{key() => value()}.
-type values() :: #{key() => value()} | value().
-type metric() :: nonempty_string().
-type state() :: tags() | no_metrics.

%%% API
-spec start(application_name(), tags()) -> {ok, pid()}.
-spec start(application_name()) -> {ok, pid()}.
start(ApplicationName, Tags) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApplicationName, Tags], []).
start(ApplicationName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApplicationName], []).

-spec write(metric(), values(), tags()) -> ok.
-spec write(metric(), values()) -> ok.
write(Name, Values, Tags) ->
    gen_server:cast(?MODULE, {metric, Name, Values, Tags}).
write(Name, Values) ->
    write(Name, Values, #{}).

%%% Gen server
-spec init([term()]) -> {ok, state()}.
init(Args) ->
    case os:getenv("SEND_METRICS", false) of
        false ->
            {ok, no_metrics};
        _ ->
            State = create_state(Args),
            write("metrics_start", 1),
            {ok, State}
    end.

-spec handle_call({metric, metric(), values(), tags()}, any(), state()) -> {reply, ok, state()}.
handle_call(_, _, no_metrics) ->
    {reply, ok, no_metrics};
handle_call({metric, Name, Values, Tags}, _From, State) ->
    do_metric(Name, Values, maps:merge(State, Tags)),
    {reply, ok, State}.

-spec handle_cast({metric, metric(), values(), tags()}, state()) -> {noreply, state()}.
handle_cast(_, no_metrics) ->
    {noreply, no_metrics};
handle_cast({metric, Name, Values, Tags}, State) ->
    do_metric(Name, Values, maps:merge(State, Tags)),
    {noreply, State}.

-spec handle_info({metric, metric(), values(), tags()}, state()) -> {noreply, state()}.
handle_info(Info, State) ->
    handle_cast(Info, State).

code_change(_, _, _) ->
    {error, unsupported}.

terminate(_, _) ->
    ok.

%%% Private

-spec create_state([term()]) -> state().
-spec create_state1(application_name(), tags()) -> state().
create_state([Application, Tags]) ->
    create_state1(Application, Tags);
create_state([Application]) ->
    create_state1(Application, #{}).
create_state1(Application, Tags) ->
    {ok, Hostname} = inet:gethostname(),
    maps:merge(#{host => Hostname, application => Application}, Tags).

-spec do_metric(metric(), values(), tags()) -> ok.
do_metric(Name, Values, Tags) when is_map(Values) ->
    influx_udp:write(Name, Values, Tags, true);
do_metric(Name, Value, Tags) when is_number(Value) ->
    influx_udp:write(Name, #{value => Value}, Tags, true).
