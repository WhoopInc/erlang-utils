-module(wutils_inflect).

-export([camel_case/1]).
-export([snake_case/1]).


%% SNAKE TO CAMEL

-spec camel_case(map() | list() | binary() | atom()) -> map().
camel_case(SnakeMap) when is_map(SnakeMap) ->
    Fun = fun(K, V, AccIn) -> maps:put(make_camel(K, false), V, AccIn) end,
    maps:fold(Fun, #{}, SnakeMap);
camel_case(SnakeString) ->
    make_camel(SnakeString, false).

% arguments are string to convert, and boolean indicating whether the previous
% character was an underscore
-spec make_camel(list() | binary() | atom(), boolean()) -> list().
make_camel(Atom, Bool) when is_atom(Atom) ->
    make_camel(atom_to_list(Atom), Bool);
make_camel(Binary, Bool) when is_binary(Binary) ->
    make_camel(binary_to_list(Binary), Bool);
make_camel("_" ++ More, _) ->
    make_camel(More, true);
make_camel([Char | More], true) ->
    [string:to_upper(Char)] ++ make_camel(More, false);
make_camel([Char | More], false) ->
    [Char] ++ make_camel(More, false);
make_camel([], _) ->
    [].


%% CAMEL TO SNAKE

-spec snake_case(map() | list() | binary() | atom()) -> map().
snake_case(CamelMap) when is_map(CamelMap) ->
    Fun = fun(K, V, AccIn) -> maps:put(make_snake(K), V, AccIn) end,
    maps:fold(Fun, #{}, CamelMap);
snake_case(CamelString) ->
    make_snake(CamelString).

% arguments are string to convert, and boolean indicating whether the previous
% character was an underscore
-spec make_snake(list() | binary() | atom()) -> list().
make_snake(Atom) when is_atom(Atom) ->
    make_snake(atom_to_list(Atom));
make_snake(Binary) when is_binary(Binary) ->
    make_snake(binary_to_list(Binary));
make_snake([Char | More]) when $A =< Char, Char =< $Z ->
    "_" ++ [string:to_lower(Char)] ++ make_snake(More);
make_snake([Char | More]) ->
    [Char] ++ make_snake(More);
make_snake([]) ->
    [].


%%% EUNIT

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_camel_test_() ->
    [
     ?_assertEqual("hello", camel_case("hello"))
    ,?_assertEqual("helloWorld", camel_case("hello_world"))
    ,?_assertEqual("HelloWorld", camel_case("__hello_world__"))
    ,?_assertEqual("hello", camel_case(hello))
    ,?_assertEqual("helloWorld", camel_case(hello_world))
    ,?_assertEqual("hello", camel_case(<<"hello">>))
    ,?_assertEqual("helloWorld", camel_case(<<"hello_world">>))
    ,?_assertEqual("HelloWorld", camel_case(<<"__hello_world__">>))
    ].

make_snake_test_() ->
    [
     ?_assertEqual("hello", snake_case("hello"))
    ,?_assertEqual("hello_world", snake_case("helloWorld"))
    ,?_assertEqual("_hello_world", snake_case("HelloWorld"))
    ,?_assertEqual("hello", snake_case(hello))
    ,?_assertEqual("hello_world", snake_case(helloWorld))
    ,?_assertEqual("hello", snake_case(<<"hello">>))
    ,?_assertEqual("hello_world", snake_case(<<"helloWorld">>))
    ,?_assertEqual("_hello_world", snake_case(<<"HelloWorld">>))
    ].

-endif.
