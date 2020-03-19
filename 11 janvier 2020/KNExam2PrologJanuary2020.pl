% 1
condition11(L) :-
    not(( nth0(U, L, AU),
          nth0(V, L, AV),
          V=:=U div 8,
          AU mod 7=\=0,
          (AU-AV-1)mod 6=\=0
        )).

condition12(L) :-
    not(( nth0(U, L, AU),
          nth0(V, L, AV),
          V=:=U div 8,
          AU mod 6=\=0,
          (AV-AU-1)mod 7=\=0
        )).

genList(0, []).
genList(N, R) :-
    N>0,
    Last is N mod 8,
    N1 is N div 8,
    genList(N1, T),
    append(T, [Last], R).

byteTreeNum(N) :-
    genList(N, L),
    condition11(L).

% 2
subsequence([], []).
subsequence([H|T], [H|R]) :-
    subsequence(T, R).
subsequence([_|T], R) :-
    subsequence(T, R).

anagrams(L) :-
    not(( member(X, L),
          member(Y, L),
          not(permutation(X, Y))
        )).

condition21(S, M) :-
    anagrams(S),
    S=[H|_],
    length(H, M),
    length(S, M1),
    M1>=M.

condition22(S, M) :-
    anagrams(S),
    S=[H|_],
    length(H, M),
    length(S, M1),
    M1>=M-2.

maxAnagrams(L, M, S) :-
    subsequence(L, S),
    condition21(S, M),
    not(( subsequence(L, S1),
          condition21(S1, M1),
          M1>M
        )).

