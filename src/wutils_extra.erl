-module(wutils_extra).

-export([
         intercalate/2
         ]).

-spec intercalate(term(), [term()]) -> [term()].
intercalate(Char, Ray) ->
    intercalate(Char, Ray, []).
intercalate(_Char, Ray, []) when length(Ray) =:= 1 ->
    Ray;
intercalate(_Char, [], Acc) ->
    lists:reverse(Acc);
intercalate(Char, [V], Acc) ->
    intercalate(Char, [], [V|Acc]);
intercalate(Char, [V|Rest], Acc) ->
    intercalate(Char, Rest, [Char|[V|Acc]]).
