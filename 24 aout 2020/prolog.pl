% Helper predicates: member, append, length.
acyclicPath(_, U, [U|P], [U|P]).
acyclicPath(E, U, [W|P], Result):- 
    U \= W, 
    takeNeighbourVertex(E, Prev, W), 
    not(member(Prev, [W|P])), 
    acyclicPath(E, U, [Prev, W|P], Result).

isConnected([V, E]):- not(( member(U, V), member(W, V), not(acyclicPath(E, U, [W], _)) )).

isAcyclic([V, E]):- not(( member(U, V), acyclicPath(E, U, [U], P), P \= [_] )).

takeNeighbourVertex(E, U, W):- member([U, W], E); member([W, U], E).

addEdgeIfNeeded([U, W], E, [[U, W]|E]):- not(member([U, W], E)).
addEdgeIfNeeded([U, W], E, E):- member([U, W], E).

removeEdgeIfNeeded([U, W], E, NewE):- removeAll(E, [U, W], NewE). 

removeAll([], _, []).
removeAll([H|T], H, R):- removeAll(T, H, R).
removeAll([H|T], X, [H|R]):- H \= X, removeAll(T, H, R).

isTree([V, E]):- isConnected([V, E]), isAcyclic([V, E]).

art_tree([V, E], [U, W]):- 
    member(U, V), member(W, V), 
    addEdgeIfNeeded([U, W], E, CandidateE), 
    isTree([V, CandidateE]).

arc_tree([V, E], [U, W]):- 
    member(U, V), member(W, V), 
    removeEdgeIfNeeded([U, W], E, CandidateE), 
    isTree([V, CandidateE]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graph([[0, 1, 2, 3, 4], [[0,1], [1, 2], [2,4], [4, 2], [2, 3], [0, 3]]]).

% Идея с покриващо дърво.

% stree(V, E, Vis, NotVis, Result).
stree(_, _, [], []).
stree(E, Vis, NVis, [[X,Y]|R]):- 
    member(X, Vis), member(Y, NVis),
    takeNeighbourVertex(X, Y, E), 
    removeAll(Y, NVis, NVis2),
    stree(E, [Y|Vis], NVis2, R).

% spanTree(Graph, Tree).
spanTree([[], []], [[], []]).
spanTree([[X|V], E], [[X|V], T]):-stree(E, [X], V, T).

acyclic([V, E]):- 
    spanTree([V, E], [V, T]), 
    not(( member([X, Y], E), not(takeNeighbourVertex(X, Y, T)) )).

arc_tree1([V, E]):-
    member([X, Y], E), 
    removeAll(E, [X, Y], E1),
    acyclic([V, E1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*If G has finitely many vertices, say n of them, 
    then the above statement is also equivalent 
    to a G is connected and has n − 1 edges.
*/
clearRepeatedEdges([], []).
clearRepeatedEdges([[U, W]|Rest], [[U, W]|Result]):- 
    clearRepeatedEdges(Rest, Result), not(member([W, U], Result)).
clearRepeatedEdges([[U, W]|Rest], Result):- 
    clearRepeatedEdges(Rest, Result), member([W, U], Result).

art_tree2([V, E], [U, W]):- 
    clearRepeatedEdges(E, NewE), 
    member(U, V), member(W, V), 
    addEdgeIfNeeded([U, W], E, CandidateE),
    length(V, N), N1 is N - 1,
    length(CandidateE, N1),
    isConnected([V, CandidateE]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper predicates: member, append, length, permutation.

generateIntersection([], _, []).
generateIntersection([H|T], B, [H|R]):- member(H, B), generateIntersection(T, B, R).
generateIntersection([H|T], B, R):- not(member(H, B)), generateIntersection(T, B, R).

subsequence([], []).
subsequence([H|T], [H|R]):- subsequence(T, R).
subsequence([_|T], R):- subsequence(T, R).

conditionExterzala(X, Y):- 
    haveCommonAtLeastNCommonElements(X, Y, 1, N),
    length(Y, N),
    haveCommonAtLeastNCommonElements(X, Y, 2, M),
    isOdd(M).

conditionExtervala(X, Y):-
    haveCommonAtLeastNCommonElements(X, Y, 1, N),
    length(Y, N),
    haveCommonAtLeastNCommonElements(X, Y, 2, M),
    isEven(M).

isOdd(M):- M mod 2 =:= 1.

isEven(M):- M mod 2 =:= 0.

isLowerBoundOkey(Int, LowerBound, 1):- length(Int, LenInt), LenInt >= LowerBound.
isLowerBoundOkey(Int, LowerBound, 0):- length(Int, LenInt), LenInt < LowerBound.

haveCommonAtLeastNCommonElements(_, [], _, 0).
haveCommonAtLeastNCommonElements(X, [H|T], LowerBound, N):- 
    haveCommonAtLeastNCommonElements(X, T, LowerBound, M),
    generateIntersection(X, H, Int),
    isLowerBoundOkey(Int, LowerBound, Bit),
    N is M + Bit.


generateCandidateX([], []).
generateCandidateX([H|T], ResultX):- 
    generateCandidateX(T, CurrentX), 
    subsequence(H, H1), 
    append(H1, CurrentX, ResultX).

exterzala(X, Y):- 
    generateCandidateX(Y, X1), permutation(X1, X), conditionExterzala(X, Y), length(X, N), 
    not(( generateCandidateX(Y, X1), conditionExterzala(X1, Y), length(X1, M), M < N )).

extervala(X, Y):-
    generateCandidateX(Y, X1), permutation(X1, X), conditionExtervala(X, Y), length(X, N), 
    not(( generateCandidateX(Y, X1), conditionExtervala(X1, Y), length(X1, M), M < N )).