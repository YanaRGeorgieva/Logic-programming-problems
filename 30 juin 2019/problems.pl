% Problem 1
isPrime(P) :-
    not(P=<1),
    P1 is P div 2,
    not(( between(2, P1, Try),
          not(P mod Try=\=0)
        )).

count(K, I, 0) :-
    PK is 6*K+1,
    PK>=I.
count(K, I, N) :-
    PK is 6*K+1,
    PK<I,
    isPrime(PK),
    K1 is K+1,
    count(K1, I, M),
    N is M+1.
    % write(K),
    % write(" = K; "),
    % write(I),
    % write(" = I "),
    % write(N),
    % write(" = N "),
    % nl. 
count(K, I, N) :-
    PK is 6*K+1,
    PK<I,
    not(isPrime(PK)),
    K1 is K+1,
    count(K1, I, N).


su(X) :-
    between(0, X, I),
    count(2, I, KsiI),
    X=:=I+KsiI,
    write(X),
    write(" = X; "),
    write(I),
    write(" = I "),
    write(KsiI),
    write(" = KsiI "),
    nl. 


% Problem 2
nat(0).
nat(N) :-
    nat(M),
    N is M+1.

do([], _, 0).
do([[K, _]|T], K, N) :-
    do(T, K, M),
    N is M+1.
do([[K1, _]|T], K, N) :-
    K1\=K,
    do(T, K, N).

di([], _, 0).
di([[_, K]|T], K, N) :-
    di(T, K, M),
    N is M+1.
di([[_, K1]|T], K, N) :-
    K1\=K,
    di(T, K, N).

pairs(A, B) :-
    nat(N),
    between(1, N, A),
    B is N-A.

genKS(1, S, [S]).
genKS(K, S, [XI|R]) :-
    K>1,
    K1 is K-1,
    between(0, S, XI),
    S1 is S-XI,
    genKS(K1, S1, R). 

generateList(L, S) :-
    pairs(K, S),
    K mod 2=:=0,
    genKS(K, S, L1),
    packTuples(L1, L).

packTuples([], []).
packTuples([Do, Di|T], [[Do, Di]|R]) :-
    packTuples(T, R).

e1g([]).
e1g(L) :-
    generateList(L, S),
    not(( between(0, S, K),
          not(condition(L, K))
        )).

condition(L, K) :-
    do(L, K, N),
    di(L, K, M),
    abs(N-M)=<1.

r(X, Y) :-
    write(X),
    write(" "),
    write(Y),
    nl,
    nl.
p(X, f(Y)) :-
    r(X, Y).
