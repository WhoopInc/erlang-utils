-module(wutils_config).

-export([get/1,get/3]).
-export_type([config/0]).

-compile({no_auto_import, [get/1]}).

-type convert_type() :: integer|string|string_list.
-type convert_result() :: atom()|integer()|string().
-type config_var() :: {atom(),atom()}|atom().
-type config() :: {string(), config_var(), convert_type(), term()}.

-spec get(config()) -> convert_result().
get(Config={_,_,_,_}) ->
    get_env(Config).

-spec get(atom(), convert_type(), term()) -> convert_result().
get(Name, Type, Default) ->
    get_env({string:to_upper(atom_to_list(Name)), Name, Type, Default}).

-spec get_env(config()) -> convert_result().
get_env(Config = {Env, _ConfigVar, Type, _Default}) ->
    case os:getenv(Env) of
        false ->
            get_config(Config);
        Val ->
            convert_val(Val, Type)
    end.

-spec get_config(config()) -> convert_result().
get_config(Config = {_Env, {App,ConfigVar}, Type, _Default}) ->
    case application:get_env(App, ConfigVar) of
        undefined ->
            get_default(Config);
        {ok, Val} ->
            convert_val(Val, Type)
    end;
get_config({Env, ConfigVar, Type, Default}) ->
    get_config({Env, {application:get_application(), ConfigVar}, Type, Default}).

-spec get_default(config()) -> convert_result().
get_default({_Env, _ConfigVar, Type, Default}) ->
    convert_val(Default, Type).

-spec convert_val(undefined|false|string()|integer(), convert_type()) -> convert_result().
convert_val(undefined, _) ->
    undefined;
convert_val(false, _) ->
    false;
convert_val(Val, integer) when is_integer(Val) ->
    Val;
convert_val(Val, integer) when is_list(Val) ->
    list_to_integer(Val);
convert_val(Val, string) when is_list(Val) ->
    Val;
convert_val(Val, string) when is_integer(Val) ->
    integer_to_list(Val).


%%% EUNIT
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_env_default_test_() ->
    [
     ?_assertEqual(1, get({"NO_SUCH_ENV", no_config, integer, "1"}))
    ,?_assertEqual(undefined, get({"NO_SUCH_ENV", no_config, integer, undefined}))
    ,?_assertEqual(false, get({"NO_SUCH_ENV", no_config, integer, false}))
    ].
-endif.
