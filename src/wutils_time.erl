-module(wutils_time).

%% Define and export a `time` and `datetime` that's like `calendar` but includes milliseconds
-export_type([
              time/0
             ,datetime/0
             ,epoch_ms/0
             ]).
-export([
         epoch_to_datetime/1
        ,datetime_to_epoch/1
        ,parse_iso/1
        ,format_iso/1
        ]).

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type millisecond() :: 0..999.

-type epoch_ms() :: non_neg_integer().

-type time() :: {hour(), minute(), second(), millisecond()}.
-type datetime() :: {calendar:date(), time()}.

%% utility functions for converting datetimes
-define(GREGORIAN_SECONDS, 62167219200).

-spec epoch_to_datetime(epoch_ms()) -> datetime().
epoch_to_datetime(EpochMs) ->
    EpochSeconds = trunc(EpochMs / 1000),
    MSecond = EpochMs - (EpochSeconds * 1000),
    {Date, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(EpochSeconds + ?GREGORIAN_SECONDS),
    {Date, {Hour, Minute, Second, MSecond}}.

-spec datetime_to_epoch(datetime()) -> epoch_ms().
datetime_to_epoch({Date, {Hour, Minute, Second, MSecond}}) ->
    GregSeconds = calendar:datetime_to_gregorian_seconds({Date, {Hour, Minute, Second}}),
    EpochSeconds = GregSeconds - ?GREGORIAN_SECONDS,
    (EpochSeconds * 1000) + MSecond.

%% this doesn't handle the full ISO format, just the complete datetime form
-spec parse_iso(string()) -> datetime().
parse_iso(DateString) ->
    {ok, [{date, Date}, {time, Time}, {offset, {TzH,TzM}}], 1} = wutils_time_lex:string(DateString),
    OffsetMSecond = -1 * (((TzH * 60) + TzM) * 60) * 1000,
    epoch_to_datetime(datetime_to_epoch({Date, Time}) + OffsetMSecond).

-spec format_iso(datetime()) -> string().
format_iso({{Year, Month, Day}, {Hour, Minute, Second, MSecond}}) ->
    lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ", [Year, Month, Day, Hour, Minute, Second, MSecond])).


%%% EUNIT
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

epoch_to_datetime_test_() ->
    [
     ?_assertEqual({{2015,5,7},{15,40,48,347}}, epoch_to_datetime(1431013248347))
    ,?_assertEqual({{1970,1,1},{0,0,0,0}}, epoch_to_datetime(0))
    ].

datetime_to_epoch_test_() ->
    [
     ?_assertEqual(1431013248347, datetime_to_epoch({{2015,5,7},{15,40,48,347}}))
    ,?_assertEqual(0, datetime_to_epoch({{1970,1,1},{0,0,0,0}}))
    ].

parse_iso_test_() ->
    [
     ?_assertEqual({{2015,5,7},{15,40,48,347}}, parse_iso("2015-05-07T15:40:48.347Z"))
    ,?_assertEqual({{2015,5,7},{15,40,48,347}}, parse_iso("2015-05-07T11:40:48.347-0400"))
    ,?_assertEqual({{1970,1,1},{0,0,0,0}}, parse_iso("1969-12-31T19:00:00.000-0500"))
    ].

format_iso_test_() ->
    [
     ?_assertEqual("2015-05-07T15:40:48.347Z", format_iso({{2015,5,7},{15,40,48,347}}))
    ,?_assertEqual("1970-01-01T00:00:00.000Z", format_iso({{1970,1,1},{0,0,0,0}}))
    ,?_assertEqual({{2015,5,7},{15,40,48,347}}, parse_iso(format_iso({{2015,5,7},{15,40,48,347}})))
    ].

-endif.