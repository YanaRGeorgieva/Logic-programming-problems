%%%% helpers: append, member, between, flatten, length
elementAt(X, 0, [X|_]).
elementAt(X, N, [_|T]) :-
    elementAt(X, M, T),
    N is M+1.

last([Last], Last).
last([_|T], R) :-
    last(T, R).

selectElement(M, L, N, AML, Matrix) :-
    Search is M*N+L,
    elementAt(AML, Search, Matrix).

select(X, L, R) :-
    append(A, [X|B], L),
    append(A, B, R).

permute([], []).
permute(L, [X|R]) :-
    select(X, L, Q),
    permute(Q, R).


prefix(P, L) :-
    append(P, _, L).

permutatedSubsequence(L, PSL) :-
    permute(L, PL),
    prefix(PSL, PL).

conditionByGroup(Matrix, K, 1) :-
    Matrix=[K|_].
conditionByGroup(Matrix, K, 2) :-
    last(Matrix, K).


condition(H, V, Matrix, N) :-
    N1 is N-1,
    not(( between(0, N, L),
          between(0, N1, M),
          M1 is M+1,
          selectElement(M, L, N, AML, Matrix),
          selectElement(M1, L, N, AM1L, Matrix),
          selectElement(L, M, N, ALM, Matrix),
          selectElement(L, M1, N, ALM1, Matrix),
          not(( member([AML, AM1L], H),
                member([ALM, ALM1], V)
              ))
        )).

tryCover(H, V, N, K, Group) :-
    flatten(H, FH),
    flatten(V, FV),
    append(FH, FV, M),
    permutatedSubsequence(M, Matrix),
    N1 is N+1,
    NxN is N1*N1,
    length(Matrix, NxN),
    conditionByGroup(Matrix, K, Group),
    condition(H, V, Matrix, N),
    prettyWriteMatrix(Matrix, N, N).

cover(H, V, N, K, Group) :-
    tryCover(H, V, N, K, Group).


prettyWriteMatrix(L, N, N) :-
    reverse(L, R),
    prettyWrite(R, N, N).

prettyWrite([], _, _) :-
    nl.
prettyWrite([H|T], N, 0) :-
    write(H),
    nl,
    prettyWrite(T, N, N).
prettyWrite([H|T], N, M) :-
    M>0,
    M1 is M-1,
    write(H),
    write(" "),
    prettyWrite(T, N, M1).