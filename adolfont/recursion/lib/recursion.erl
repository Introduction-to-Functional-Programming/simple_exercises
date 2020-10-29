-module recursion.

-export [fac/1, fac2/1, fib/1, fib2/1].

fac(1) ->
    1;
fac(N) ->
    fac(N - 1) * N.

fac2(N) ->
    fac2(N, 1).
fac2(1, Current) ->
    Current;
fac2(N, Current) ->
    fac2(N - 1, N * Current).

fib(1) ->
    1;
fib(2) ->
    1;
fib(N) ->
    fib(N - 1) + fib(N - 2).


% Comparar a partir de 40
fib2(N) when is_integer(N) andalso (N > 0) ->
    fib2(N, 0, 1).

fib2(1, _Acc1, Acc2) ->
    Acc2;
fib2(N, Acc1, Acc2) ->
    fib2(N - 1, Acc2, Acc1 + Acc2).


