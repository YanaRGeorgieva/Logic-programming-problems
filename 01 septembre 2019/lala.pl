g([], []).
g(Y, [A|R]) :-
    append(A, B, Y),
    A\=[],
    g(B, R).

p(X, Y) :-
    member(Y, X),
    g(Y, L),
    not(( member(V, L),
          not(member(V, X))
        )), write(L).
