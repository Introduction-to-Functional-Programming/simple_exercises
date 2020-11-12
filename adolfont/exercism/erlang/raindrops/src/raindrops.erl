-module(raindrops).

-export([convert/1]).

convert(Number) ->
    case convert(Number, [3, 5, 7]) of
        [] ->
            integer_to_list(Number);
        Result ->
            Result
    end.

convert(_Number, []) ->
    "";
convert(Number, [Head | Tail]) ->
    show(Number, Head) ++ convert(Number, Tail).

show(Number, Dividend) ->
    show(Number, Dividend, Number rem Dividend).

show(_Number, Dividend, 0) ->
    name(Dividend);
show(_Number, _Dividend, _) ->
    "".

name(3) ->
    "Pling";
name(5) ->
    "Plang";
name(7) ->
    "Plong".
