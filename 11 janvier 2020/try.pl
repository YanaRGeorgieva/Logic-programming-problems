nat(0).
nat(N):- nat(M), N is M + 1.

index(I, S):- nat(I), length(S, L), I < L.