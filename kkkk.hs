listan(N,L):-
     N1 is N-1,
     listan(N1,L1),
     append(L1,[N],L).
listan(0,[]).