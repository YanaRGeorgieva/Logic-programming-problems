%Helpers: length, between
sum([], 0).
sum([H|T], N) :-
    sum(T, M),
    N is M+H.

isSquare(X) :-
    between(0, X, X1),
    X1*X1=:=X.

squareList(L) :-
    length(L, N),
    sum(L, S),
    isSquare(N),
    isSquare(S).

isCube(X) :-
    between(0, X, X1),
    X1*X1*X1=:=X.

cubeList(L) :-
    length(L, N),
    sum(L, S),
    isCube(N),
    isCube(S).


% Helpers: length, member.
subsequence([], []).
subsequence([H|T], [H|R])   :-
    subsequence(T, R).
subsequence([_|T], R) :-
    subsequence(T, R).

condition1(M) :-
    not(( member([APX, BPX, AQX, BQX], M),
          member([APY, BPY, AQY, BQY], M),
          (APX/BPX-APY/BPY)^2+(AQX/BQX-AQY/BQY)^2=<2
        )).

max_independent(S, M) :-
    subsequence(M, S),
    condition1(M),
    length(M, N),
    not(( subsequence(M1, S),
          condition1(M1),
          length(M1, N1),
          N1>N
        )).

condition2(S, M) :-
    not(( member([APX, BPX, AQX, BQX], S),
            member([APY, BPY, AQY, BQY], M),
            (APX/BPX-APY/BPY)^2+(AQX/BQX-AQY/BQY)^2>1
        )).

min_cover(S, M) :-
    subsequence(M, S),
    condition2(S, M),
    length(M, N),
    not(( subsequence(M1, S),
          condition2(S, M1),
          length(M1, N1),
          N>N1
        )).

