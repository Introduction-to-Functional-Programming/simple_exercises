-module(strain).

-export([keep/2, discard/2]).

keep(F, List) ->
    aux(F, List, true).

discard(F, List) ->
    aux(F, List, false).

aux(_Fn, [], _) ->
    [];
aux(Fn, [Head | Tail], Value) ->
    case Fn(Head) of
        Value ->
            [Head | aux(Fn, Tail, Value)];
        _ ->
            aux(Fn, Tail, Value)
    end.
