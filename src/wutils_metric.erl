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

%%% API
start(ApplicationName, Tags) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApplicationName, Tags], []).
start(ApplicationName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApplicationName], []).

write(Name, Value, Tags) ->
    gen_server:cast(?MODULE, {metric, Name, Value, Tags}).
write(Name, Value) ->
    write(Name, Value, #{}).

%%% Gen server

init(Args) ->
    State = create_state(Args),
    write("metrics.start", 1),
    {ok, State}.

handle_call({metric, Name, Value, Tags}, _From, State) ->
    do_metric(Name, Value, maps:merge(State, Tags)),
    {reply, ok, State}.

handle_cast({metric, Name, Value, Tags}, State) ->
    do_metric(Name, Value, maps:merge(State, Tags)),
    {noreply, State}.

handle_info({metric, Name, Value, Tags}, State) ->
    do_metric(Name, Value, maps:merge(State, Tags)),
    {noreply, State}.

code_change(_, _, _) ->
    {error, unsupported}.

terminate(_, _) ->
    ok.

%%% Private

create_state([Application, Tags]) ->
    create_state1(Application, Tags);
create_state([Application]) ->
    create_state1(Application, #{}).
create_state1(Application, Tags) ->
    {ok, Hostname} = inet:gethostname(),
    maps:merge(#{host => Hostname, application => Application}, Tags).


do_metric(Name, Value, Tags) ->
    influx_udp:write(Name, #{value => Value}, Tags, true).
