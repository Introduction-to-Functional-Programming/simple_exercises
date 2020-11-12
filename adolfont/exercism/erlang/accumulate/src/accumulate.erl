-module(accumulate).

-export([accumulate/2]).

accumulate(Fn, Ls) ->
    accumulate(Fn, Ls, []).

accumulate(Fn, [Head | Tail], Result) ->
    accumulate(Fn, Tail, [Fn(Head) | Result]);
accumulate(_Fn, [], Result) ->
    lists:reverse(Result).
