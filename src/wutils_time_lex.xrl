%% -*- mode: erlang; -*-

Definitions.

D = [0-9]

Rules.

{D}{D}{D}{D}-{D}{D}-{D}{D}      : {token, {date, chop_ints(TokenChars, [[1,4],[6,7],[9,10]])}}.
{D}{D}{D}{D}{D}{D}{D}{D}        : {token, {date, chop_ints(TokenChars, [[1,4],[5,6],[7,8]])}}.

{D}{D}:{D}{D}:{D}{D}\.{D}{D}{D} : {token, {time, ms_to_float(chop_ints(TokenChars, [[1,2],[4,5],[7,8],[10,12]]))}}.
{D}{D}:{D}{D}:{D}{D}            : {token, {time, ms_to_float(erlang:append_element(chop_ints(TokenChars, [[1,2],[4,5],[7,8]]), 0))}}.

Z                               : {token, {offset, {0,0}}}.
[-+]{D}{D}{D}{D}                : {token, {offset, chop_ints(TokenChars, [[1,3],[4,5]])}}.

[\t\sT]                         : skip_token.

Erlang code.

ms_to_float({H, M, S, 0}) ->
    {H, M, erlang:float(S)};
ms_to_float({H, M, S, Ms}) ->
    {H, M, S + (Ms/1000)}.

chop_ints(String, Divisions) ->
    Parts = chop(String, Divisions),
    list_to_tuple(lists:map(fun erlang:list_to_integer/1, Parts)).

chop(String, Divisions) ->
    lists:reverse(chop(String, Divisions, [])).
chop(_String, [], Acc) ->
    Acc;
chop(String, [Div|Rest], Acc) ->
    chop(String, Rest, [apply(string, sub_string, [String|Div])|Acc]).
