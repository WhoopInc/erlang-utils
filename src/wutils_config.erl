-module(wutils_config).

-export([get/1]).
-export_type([config/0]).

-compile({no_auto_import,[get/1]}).

-type config() :: {string(), atom(), atom(), term()}.

-spec get(config()) -> term().
get(Config) ->
    get_env(Config).

get_env(Config = {Env, _ConfigVar, Type, _Default}) ->
    case os:getenv(Env) of
        false ->
            get_config(Config);
        Val ->
            convert_val(Val, Type)
    end.

get_config(Config = {_Env, {App,ConfigVar}, Type, _Default}) ->
    case application:get_env(App, ConfigVar) of
        undefined ->
            get_default(Config);
        Val ->
            convert_val(Val, Type)
    end;
get_config({Env, ConfigVar, Type, Default}) ->
    get_config({Env, {application:get_application(), ConfigVar}, Type, Default}).

get_default({_Env, _ConfigVar, Type, Default}) ->
    convert_val(Default, Type).

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
    ].
-endif.
