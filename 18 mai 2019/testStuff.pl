natural(0).
natural(N) :-
    natural(M),
    N is M+1.

even(N) :-
    length(X, N),
    Y=[0, 1|Y],
    append(X, [0|_], Y).


genList(_, []).
genList(X, [A]) :-
    member(A, X).
genList(X, [H|T]) :-
    genList(X, T),
    member(H, X).

cyclicy(X) :-
    natural(N),
    genList(X, Y),
    length(Y, M), % u.u | u.u.y for y prefix of u
    M>N,
    append(Y, Z, Z),
    append(X, _, Z).