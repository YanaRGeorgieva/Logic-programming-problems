% Helper predicates: member, length.

balanced(V, E):- 
    length(V, LV), logarithm(LV, 2, Lim), Limit is 2 * Lim, 
    checkMaxDepthBelow(V, E, Limit).

logarithm(1, _, 0).
logarithm(N, Base, Res):-
    N > 1, N1 is N div Base,
    logarithm(N1, Base, M),
    Res is M + 1.

acyclicPath(_, U, [U|P], [U|P]).
acyclicPath(E, U, [W|P], Result):- 
    U \= W, 
    takeNeighbourVertex(E, Prev, W), 
    not(member(Prev, [W|P])), 
    acyclicPath(E, U, [Prev, W|P], Result).

takeNeighbourVertex(E, U, W):- member([U, W], E); member([W, U], E).

getRoot(V, E, Root):- member(Root, V), not(member([_, V], E)).

checkMaxDepthBelow(V, E, Limit):- 
    getRoot(V, E, Root), 
    not(( member(U, V), acyclicPath(E, Root, [U], P), length(P, LP), LP > Limit )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates: append, permutation.

runOrDie(Towers, N, L):- 
    permutation(Towers, L), 
    pillarsToNumbers(L, NumL), 
    condition(NumL, N).

pillarsToNumbers([], []).
pillarsToNumbers([H|T], [NumH|R]):- 
    pillarsToNumbers(T, R), countNestedness(H, NumH).

countNestedness([], 0).
countNestedness([H|[]], N):- 
    countNestedness(H, M), N is M + 1.

condition(NumL, N):- 
    append([First|_], [Last], NumL), 
    R1 is abs(N - First), R1 =< 3,
    R2 is abs(N - Last), R2 =< 3,
    not(( append(_, [A, B|_], NumL), R3 is abs(B - A), R3 > 3 )).

/*
?- runOrDie([[[[[]]]],[[]],[[[[[[]]]]]]], 4, L).
L = [[[]], [[[[]]]], [[[[[[]]]]]]] ;
L = [[[[[[[]]]]]], [[[[]]]], [[]]] ;
false.
*/

