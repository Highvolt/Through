% Autor:
% Data: 05-11-2011

delete2(X,L,DL):-del2(X,L,[],DL).

del2(X,[X|T],A,DL):-rev_append(A,T,DL).
del2(X,[Y|T],A,DL):-del2(X,T,[Y|A],DL).
% append a list to a reverted list, e.g. rev_append([2,1],[3,4],[1,2,3,4])
rev_append([],L,L).
rev_append([H|T],L,LT):-rev_append(T,[H|L],LT).

choose([], []).
choose([],_) :- !, fail.
choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).
        
appender(X,Y,L):- append([X],[Y],L).
        
generate(X,Y,L,I,K):- I =:= 0 -> append(L,[],K), !;
                  choose(X,Z), choose(Y,M),appender(Z,M,T), append([T],L,NovaL), delete2(Z,X,NovaX), delete2(M,Y,NovaY), generate(NovaX,NovaY,NovaL,I-1,K).

%chamada de teste generatePalmPlaces([1,2,3,4,13,14],[5,6,7,8,9,10],[10,20,10,40],[],K).
generatePalmPlaces(X,Y,P,L,K):- length(P,I),
                          generate(X,Y,L,I,K),!.
