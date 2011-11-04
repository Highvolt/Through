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


%inicializador do jogador 1 joga com as pecas impares.
initplayer(L,C,P,1):-L=[1,3,5,7,9],createnewhand(11,[],C),P=[].
%inicializador do jogador 2 joga com as pecas pares.
initplayer(L,C,P,2):-L=[0,2,4,6,8],createnewhand(10,[],C),P=[].
getPlayer(X,2):-N is X/2,(integer(N)->true;false).
getPlayer(X,1):-not(getPlayer(X,2)).

append([X|Y],Z,[X|W]) :- append(Y,Z,W).
append([],X,X).

addcamel(N,C,LC,D):-N1 is N-1, (N1>=0->append(LC,[C],R),addcamel(N1,C,R,D);D=LC).


%retira camelos usage takeout(camelo,lista de camelos, retorno)
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

createnewhand(N,LC,D):-N1 is N-2,(N1>=0->addcamel(10,N1,LC,R),createnewhand(N1,R,D);D=LC).



run :- 
initialBoard(A), printboard(A).

printboard([]).
printboard([Head|Tail]):- rowanalise(Head),write('\n'), printboard(Tail).
rowanalise([Head|Tail]):-N is Head/2, (integer(N) -> write(' /'); write(' \\_/')), printrow(Tail).
printrow([]).
printrow([Head|Tail]):-(Head=e->write(' ');write(Head)), write('\\_/'),printrow(Tail).

