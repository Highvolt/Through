initialBoard(
[[0,0,0,0,0,0,0,0,0,0,0,0],
[1,0,0,e,e,e,0,0,e,e,0,0],
[2,0,0,e,e,e,e,e,e,e,e,0],
[3,0,e,e,e,e,e,e,e,e,e,0],
[4,0,0,e,e,e,e,e,e,e,e,0],
[5,0,e,e,e,e,e,e,e,e,e,0],
[6,0,e,e,e,e,e,e,e,e,e,0],
[7,0,e,e,e,e,e,e,e,e,e,0],
[8,0,e,e,e,e,e,e,e,e,e,0],
[9,0,e,e,e,e,e,e,e,e,e,0],
[10,0,e,e,e,e,e,e,e,e,e,0],
[11,0,e,e,e,e,e,e,e,e,e,0],
[12,0,e,e,e,e,e,e,e,e,e,0],
[13,0,e,e,0,e,e,e,e,e,e,0],
[14,0,e,e,e,0,e,e,e,e,e,0],
[15,0,e,e,0,0,0,e,e,e,e,0],
[16,0,e,e,e,0,0,e,e,e,e,0],
[17,0,e,e,e,e,e,e,e,e,0,0],
[18,0,e,e,e,e,e,e,e,e,e,0],
[19,0,e,e,e,e,e,e,e,e,0,0],
[20,0,0,e,e,e,e,e,e,e,0,0],
[21,0,e,e,e,e,e,e,e,0,0,0],
[22,0,0,e,e,e,e,e,e,0,0,0],
[23,0,0,e,e,e,e,e,0,0,0,0],
[24,0,0,0,e,e,e,e,0,0,0,0],
[25,0,0,0,e,e,e,0,0,0,0,0],
[26,0,0,0,0,e,e,0,0,0,0,0],
[27,0,0,0,0,0,0,0,0,0,0,0]]
).

%list handlers
getpos(X,Y,V,B):-X1 is X+1,(X1>=0->(Y>=0->getrow(Y,L,B),!,getrow(X1,V,L);false);false). %x+1 por causa da label
getrow(Y,L,B):-getrow(Y,L,0,B).
getrow(Y,L,A,[H|T]):-(A==Y->L=H;N is A+1,getrow(Y,L,N,T)).

selrow(Y,B,H1,T1,L):-selrow(Y,0,B,H1,T1,[],L).
selrow(Y,A,[H|T],H1,T1,Acc,L):-(A==Y->T1=T,H1=Acc,L=H;N is A+1,append(Acc,[H],Q),selrow(Y,N,T,H1,T1,Q,L)).


setpos(X,Y,V,B,G):-X1 is X+1,(X1>=0->(Y>=0->selrow(Y,B,H1,T1,L),setpos(X1,L,V,NL),append(H1,[NL],W),append(W,T1,G);false);false).
setpos(Y,B,V,G):-setpos(Y,0,V,B,[],G).
setpos(Y,A,V,[H|T],Acc,G):-(A==Y->append(Acc,[V],B),append(B,T,G);N is A+1,append(Acc,[H],P),setpos(Y,N,V,T,P,G)).

append([X|Y],Z,[X|W]) :- append(Y,Z,W).
append([],X,X).

%Board Handler.
vizinho(1,X,Y,B,Valor):-Y1 is Y-2,!, getpos(X,Y1,Valor,B).
vizinho(2,X,Y,B,Valor):-Y1 is Y+2,!, getpos(X,Y1,Valor,B).
vizinho(3,X,Y,B,Valor):-Y1 is Y-1,(even(X)->X1=X;X1 is X-1),!, getpos(X1,Y1,Valor,B).
vizinho(4,X,Y,B,Valor):-Y1 is Y-1,(even(X)->X1 is X+1;X1 is X),!, getpos(X1,Y1,Valor,B).
vizinho(5,X,Y,B,Valor):-Y1 is Y+1,(even(X)->X1=X;X1 is X-1),!, getpos(X1,Y1,Valor,B).
vizinho(6,X,Y,B,Valor):-Y1 is Y+1,(even(X)->X1 is X+1;X1 is X),!, getpos(X1,Y1,Valor,B).
vizinho(7,X,Y,B,Valor):-X1 is X-1,!,getpos(X1,Y,Valor,B).
vizinho(8,X,Y,B,Valor):-X1 is X+1,!, getpos(X1,Y,Valor,B).

printboard([]).
printboard([Head|Tail]):- rowanalise(Head),write('\n'), printboard(Tail).
rowanalise([Head|Tail]):-N is Head/2, (integer(N) -> write(' /'); write(' \\_/')), printrow(Tail).
printrow([]).
printrow([Head|Tail]):-(Head=e->write(' ');write(Head)), write('\\_/'),printrow(Tail).



%misc
even(X):-N is X/2,(integer(N)->true;false).

%camelos
mesmacor(X,Y):- (X==Y->true;(even(X)->N is X+1,N==Y;N is X-1,N==Y)).
addcamel(N,C,LC,D):-N1 is N-1, (N1>=0->append(LC,[C],R),addcamel(N1,C,R,D);D=LC).
%retira camelos usage takeout(camelo,lista de camelos, retorno)
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

%inicializador do jogador 1 joga com as pecas impares.
initplayer(L,C,P,1):-L=[1,3,5,7,9],createnewhand(11,[],C),P=[].
%inicializador do jogador 2 joga com as pecas pares.
initplayer(L,C,P,2):-L=[0,2,4,6,8],createnewhand(10,[],C),P=[].
getPlayer(X,2):-N is X/2,(integer(N)->true;false).
getPlayer(X,1):-not(getPlayer(X,2)).


createnewhand(N,LC,D):-N1 is N-2,(N1>=0->addcamel(10,N1,LC,R),createnewhand(N1,R,D);D=LC).

%gamestate
fase(L1,L2):-(L1==[]->(L2==[]->false;true);true). %true -> fase de colocacao de lideres %false -> fase de colocacao de camelos.

fim(C1,C2):-(C1==[]->(C2==[]->true;false);false). %true -> fim do jogo.
%fase([],[]):-false. %fase 2

%main rotine
run :- 
initialBoard(A), printboard(A).


