-module(g10).
-compile({parse_transform, etc}).

% list computations

reverse(L) -> reverse(L,[]).

reverse([],R) -> R;
reverse([H|T],R) -> reverse(T,[H|R]).


append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.


tail_recursive_fib(N) ->
    tail_recursive_fib(N, 0, 1, []).

tail_recursive_fib(0, _Current, _Next, Fibs) ->
    reverse(Fibs);
tail_recursive_fib(N, Current, Next, Fibs) -> 
    tail_recursive_fib(N - 1, Next, Current + Next, [Current|Fibs]).


sum(L) -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

nubSorted([]) -> [];
nubSorted([X]) -> [X];
nubSorted([X|[X|Xs]]) -> nubSorted([X|Xs]);
nubSorted([X|Xs]) -> [X | nubSorted(Xs)].