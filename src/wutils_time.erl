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
        ,shift/3
        ,datetime_to_erlang_datetime/1
        ,erlang_datetime_to_datetime/1
        ]).

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: number().

-type epoch_ms() :: non_neg_integer().

-type time() :: {hour(), minute(), second()}.
-type datetime() :: {calendar:date(), time()}.

%% utility functions for converting datetimes
-define(GREGORIAN_SECONDS, 62167219200).
-define(MILLISECONDS_PER_SECOND, 1000).
-define(MILLISECONDS_PER_MINUTE, ?MILLISECONDS_PER_SECOND * 60).
-define(MILLISECONDS_PER_HOUR, ?MILLISECONDS_PER_MINUTE * 60).

-spec epoch_to_datetime(epoch_ms()) -> datetime().
epoch_to_datetime(EpochMs) ->
    EpochSeconds = trunc(EpochMs / 1000),
    MSecond = (EpochMs - (EpochSeconds * 1000))/1000,
    {Date, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(EpochSeconds + ?GREGORIAN_SECONDS),
    {Date, {Hour, Minute, Second+MSecond}}.

-spec datetime_to_epoch(datetime()) -> epoch_ms().
datetime_to_epoch({Date, {Hour, Minute, Second}}) ->
    Second1 = trunc(Second),
    MSecond = trunc((Second - Second1) * 1000),
    GregSeconds = calendar:datetime_to_gregorian_seconds({Date, {Hour, Minute, Second1}}),
    EpochSeconds = GregSeconds - ?GREGORIAN_SECONDS,
    (EpochSeconds * 1000) + MSecond.

%% this doesn't handle the full ISO format, just the complete datetime form
-spec parse_iso(string()|binary()) -> datetime().
parse_iso(DateString) when is_binary(DateString) ->
    parse_iso(binary_to_list(DateString));
parse_iso(DateString) ->
    {ok, [{date, Date}, {time, Time}, {offset, {TzH,TzM}}], 1} = wutils_time_lex:string(DateString),
    OffsetMSecond = -1 * (((TzH * 60) + TzM) * 60) * 1000,
    epoch_to_datetime(datetime_to_epoch({Date, Time}) + OffsetMSecond).

-spec format_iso(datetime()) -> string().
format_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Second1 = trunc(Second),
    MSecond = trunc((Second - Second1) * 1000),
    lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0bZ", [Year, Month, Day, Hour, Minute, Second1, MSecond])).

-spec datetime_to_erlang_datetime(datetime()) -> {calendar:datetime(), number()}.
datetime_to_erlang_datetime({Date, {H,M,S}}) ->
    SS = trunc(S),
    MS = S - SS,
    {{Date, {H,M,SS}}, MS}.
-spec erlang_datetime_to_datetime({calendar:datetime(), number()}) -> datetime().
erlang_datetime_to_datetime({{Date, {H,M,S}}, Ms}) ->
    {Date, {H,M,S+Ms}}.

-spec shift(datetime(), integer(), years|days|hours|minutes|seconds|milliseconds) -> datetime().
shift({{YY,MM,DD},T}, Change, years) ->
    {{YY + Change, MM, DD}, T};
%shift({{YY,MM,DD},T}, Change, months) ->
%    D = Change/12,
%    Dy = trunc(D),
%    Dm = round((D - Dy)*12),  %% "round" to avoid rounding errors. Should be trunc() if I knew that was safe
%    {{YY + Dy, MM + Dm, DD}, T};
shift({Date, Time}, Change, days) ->
    DateShifted = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Change),
    {DateShifted, Time};
shift(Datetime, Change, hours) ->
    shift(Datetime, Change * ?MILLISECONDS_PER_HOUR, milliseconds);
shift(Datetime, Change, minutes) ->
    shift(Datetime, Change * ?MILLISECONDS_PER_MINUTE, milliseconds);
shift(Datetime, Change, seconds) ->
    shift(Datetime, Change * ?MILLISECONDS_PER_SECOND, milliseconds);
shift(Datetime, Change, milliseconds) ->
    Epoch = datetime_to_epoch(Datetime),
    ShiftedEpoch = Epoch + Change,
    epoch_to_datetime(ShiftedEpoch).



%%% EUNIT
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

epoch_to_datetime_test_() ->
    [
     ?_assertEqual({{2015,5,7},{15,40,48.347}}, epoch_to_datetime(1431013248347))
    ,?_assertEqual({{1970,1,1},{0,0,0.0}}, epoch_to_datetime(0))
    ].

datetime_to_epoch_test_() ->
    [
     ?_assertEqual(1431013248347, datetime_to_epoch({{2015,5,7},{15,40,48.347}}))
    ,?_assertEqual(0, datetime_to_epoch({{1970,1,1},{0,0,0.0}}))
    ,?_assertEqual(0, datetime_to_epoch({{1970,1,1},{0,0,0}}))
    ,?_assertEqual(1433004879000, datetime_to_epoch({{2015,5,30},{16,54,39}}))
    ].

parse_iso_test_() ->
    [
     ?_assertEqual({{2015,5,7},{15,40,48.347}}, parse_iso("2015-05-07T15:40:48.347Z"))
    ,?_assertEqual({{2015,5,7},{15,40,48.347}}, parse_iso("2015-05-07T11:40:48.347-0400"))
    ,?_assertEqual({{2015,5,7},{15,40,48.347}}, parse_iso(<<"2015-05-07T11:40:48.347-0400">>))
    ,?_assertEqual({{2015,6,9},{19,30,56.0}}, parse_iso(<<"2015-06-09T19:30:56Z">>))
    ,?_assertEqual({{1970,1,1},{0,0,0.0}}, parse_iso("1969-12-31T19:00:00.000-0500"))
    ].

format_iso_test_() ->
    [
     ?_assertEqual("2015-05-07T15:40:48.347Z", format_iso({{2015,5,7},{15,40,48.347}}))
    ,?_assertEqual("1970-01-01T00:00:00.000Z", format_iso({{1970,1,1},{0,0,0.0}}))
    ,?_assertEqual({{2015,5,7},{15,40,48.347}}, parse_iso(format_iso({{2015,5,7},{15,40,48.347}})))
    ].

shift_test_() ->
    [
     ?_assertEqual({{2016,1,1},{0,0,0}}, shift({{2015,1,1},{0,0,0}}, 1, years))
    ,?_assertEqual({{2014,1,1},{0,0,0}}, shift({{2015,1,1},{0,0,0}}, -1, years))
%    ,?_assertEqual({{2015,2,1},{0,0,0}}, shift({{2015,1,1},{0,0,0}}, 1, months))
%    ,?_assertEqual({{2014,12,1},{0,0,0}}, shift({{2015,1,1},{0,0,0}}, -1, months))
    ,?_assertEqual({{2015,1,2},{0,0,0}}, shift({{2015,1,1},{0,0,0}}, 1, days))
    ,?_assertEqual({{2014,12,31},{0,0,0}}, shift({{2015,1,1},{0,0,0}}, -1, days))
    ,?_assertEqual({{2015,1,1},{1,0,0.0}}, shift({{2015,1,1},{0,0,0}}, 1, hours))
    ,?_assertEqual({{2014,12,31},{23,0,0.0}}, shift({{2015,1,1},{0,0,0}}, -1, hours))
    ].

-endif.
