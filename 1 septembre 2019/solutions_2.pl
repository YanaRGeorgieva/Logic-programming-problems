insert(X, L, R) :-
    append(A, B, L),
    append(A, [X|B], R).

permute([], []).
permute([H|T], R) :-
    permute(T, Q),
    insert(H, Q, R).

isCyclic(L) :-
    append(UL, V, L),
    cyclicConcatenations(UL, U),
    U\=[],
    append(V, W, U),
    writeThem(L, UL, U, V, W).

writeThem(L, UL, U, V, W) :-
    write("L: "),
    write(L),
    write(". UL: "),
    write(UL),
    write(". V: "),
    write(V),
    write(". U: "),
    write(U),
    write(". W: "),
    write(W).

cyclicConcatenations(UL, U) :-
    length(UL, N),
    tryMultiConcat(N, N, UL, U).

removeDuplicates([], []).
removeDuplicates([H|T], [H|R]) :-
    not(member(H, R)),
    removeDuplicates(T, R).
removeDuplicates([H|T], R) :-
    member(H, R),
    removeDuplicates(T, R).

tryMultiConcat(M, N, UL, U) :-
    M>0,
    N mod M=:=0,
    divideInEqualListSizes(UL, M, M, DivUL),
    removeDuplicates(DivUL, [U]).
tryMultiConcat(M, N, UL, U) :-
    M>0,
    M1 is M-1,
    tryMultiConcat(M1, N, UL, U).

divideInEqualListSizes([], 0, _, []).
divideInEqualListSizes([H|T], Cnt, M, [Curr|R]) :-
    Cnt>0,
    Cnt1 is Cnt-1,
    getMElementList([H|T], M, Curr, Rest),
    divideInEqualListSizes(Rest, Cnt1, M, R).

getMElementList(Rest, 0, [], Rest).
getMElementList([H|T], N, [H|R], Rest) :-
    N>0,
    N1 is N-1,
    getMElementList(T, N1, R, Rest).

prefix(P, L) :-
    append(P, _, L).

cycl(A, L) :-
    permute(A, PA),
    prefix(L, PA),
    isCyclic(L).