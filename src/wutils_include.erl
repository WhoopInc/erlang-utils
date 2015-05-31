-module(wutils_include).
-include("wutils.hrl").

%%% EUNIT
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lager_test_() ->
    {setup,
     fun () -> application:ensure_all_started(lager) end,
     [
      ?_assertEqual(ok, ?DEBUG("test-print debug"))
     ,?_assertEqual(ok, ?DEBUG("test-print debug: ~p", [arg]))
     ,?_assertEqual(ok, ?INFO("test-print info"))
     ,?_assertEqual(ok, ?INFO("test-print info: ~p", [arg]))
     ,?_assertEqual(ok, ?WARN("test-print warning"))
     ,?_assertEqual(ok, ?WARN("test-print warning: ~p", [arg]))
     ]
    }.

-endif.
