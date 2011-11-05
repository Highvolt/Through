:-dynamic getTab/1.

:-dynamic getL/2.


fase2tab([[0, w, w, w, w, w, w, w, w, w, w, w], [1, w, w, e, 1, e, w, w, e, e, w, w], [2, w, w, e, e, e, e, 5, e, e, e, w], [3, w, e, e, e, 3, e, e, e, e, e, w], [4, w, w, e, e, e, e, e, e, e, e, w], [5, w, e, e, e, e, 0, e, e, e, e, w], [6, w, e, e, e, e, e, e, e, e, e, w], [7, w, e, e, 2, e, e, e, e, e, e, w], [8, w, e, e, e, e, e, e, e, e, e, w], [9, w, e, e, e, e, e, e, e, 6, e, w], [10, w, e, e, e, e, e, e, e, e, e, w], [11, w, e, e, e, e, e, e, e, e, e, w], [12, w, e, e, 7, e, e, e, e, e, e, w], [13, w, e, e, w, e, e, e, e, e, e, w], [14, w, e, e, e, w, e, e, e, e, e, w], [15, w, e, e, w, w, w, e, e, e, e, w], [16, w, e, 4, e, w, w, e, e, e, e, w], [17, w, e, e, e, e, e, e, e, e, w, w], [18, w, e, e, e, e, e, e, e, e, e, w], [19, w, e, e, e, e, e, e, e, e, w, w], [20, w, w, e, e, e, e, e, e, 9, w, w], [21, w, e, e, e, e, e, 8, e, w, w, w], [22, w, w, e, e, e, e, e, e, w, w, w], [23, w, w, e, e, e, e, e, w, w, w, w], [24, w, w, w, e, e, e, e, w, w, w, w], [25, w, w, w, e, e, e, w, w, w, w, w], [26, w, w, w, w, e, e, w, w, w, w, w], [27, w, w, w, w, w, w, w, w, w, w, w]]).


initialBoard(
[[0,w,w,w,w,w,w,w,w,w,w,w],
[1,w,w,e,e,e,w,w,e,e,w,w],
[2,w,w,e,e,e,e,e,e,e,e,w],
[3,w,e,e,e,e,e,e,e,e,e,w],
[4,w,w,e,e,e,e,e,e,e,e,w],
[5,w,e,e,e,e,e,e,e,e,e,w],
[6,w,e,e,e,e,e,e,e,e,e,w],
[7,w,e,e,e,e,e,e,e,e,e,w],
[8,w,e,e,e,e,e,e,e,e,e,w],
[9,w,e,e,e,e,e,e,e,e,e,w],
[10,w,e,e,e,e,e,e,e,e,e,w],
[11,w,e,e,e,e,e,e,e,e,e,w],
[12,w,e,e,e,e,e,e,e,e,e,w],
[13,w,e,e,w,e,e,e,e,e,e,w],
[14,w,e,e,e,w,e,e,e,e,e,w],
[15,w,e,e,w,w,w,e,e,e,e,w],
[16,w,e,e,e,w,w,e,e,e,e,w],
[17,w,e,e,e,e,e,e,e,e,w,w],
[18,w,e,e,e,e,e,e,e,e,e,w],
[19,w,e,e,e,e,e,e,e,e,w,w],
[20,w,w,e,e,e,e,e,e,e,w,w],
[21,w,e,e,e,e,e,e,e,w,w,w],
[22,w,w,e,e,e,e,e,e,w,w,w],
[23,w,w,e,e,e,e,e,w,w,w,w],
[24,w,w,w,e,e,e,e,w,w,w,w],
[25,w,w,w,e,e,e,w,w,w,w,w],
[26,w,w,w,w,e,e,w,w,w,w,w],
[27,w,w,w,w,w,w,w,w,w,w,w]]
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
vizinho(3,X,Y,B,Valor):-Y1 is Y-1,!,(even(Y)->X1 is X-1;X1 is X),!, getpos(X1,Y1,Valor,B).
vizinho(4,X,Y,B,Valor):-Y1 is Y-1,!,(even(Y)->X1 is X;X1 is X+1),!, getpos(X1,Y1,Valor,B).
vizinho(5,X,Y,B,Valor):-Y1 is Y+1,!,(even(Y)->X1 is X-1;X1 is X),!, getpos(X1,Y1,Valor,B).
vizinho(6,X,Y,B,Valor):-Y1 is Y+1,!,(even(Y)->X1 is X;X1 is X+1),!, getpos(X1,Y1,Valor,B).
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
mesmacor(X,Y):- (X==Y->true;(even(X)->N is X+1,Y=N;N is X-1,Y=N)).
addcamel(N,C,LC,D):-N1 is N-1, (N1>=0->append(LC,[C],R),addcamel(N1,C,R,D);D=LC).
%retira camelos usage takeout(camelo,lista de camelos, retorno)
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

%inicializador do jogador 1 joga com as pecas impares.
initplayer(2):-initplayer(L,C,P,2),assert(getL(2,L)),assert(getC(2,C)),assert(getP(2,P)).
initplayer(1):-initplayer(L,C,P,1),assert(getL(1,L)),assert(getC(1,C)),assert(getP(3,P)).
initplayer(L,C,P,1):-L=[1,3,5,7,9],createnewhand(11,[],C),P=[].
%inicializador do jogador 2 joga com as pecas pares.
initplayer(L,C,P,2):-L=[0,2,4,6,8],createnewhand(10,[],C),P=[].
getPlayer(X,2):-N is X/2,(integer(N)->true;false).
getPlayer(X,1):-not(getPlayer(X,2)).


createnewhand(N,LC,D):-N1 is N-2,(N1>=0->addcamel(10,N1,LC,R),createnewhand(N1,R,D);D=LC).

%gamestate
fase:- getL(1,L1),getL(2,L2),(L1==[]->(L2==[]->false;true);true). %true -> fase de colocacao de lideres %false -> fase de colocacao de camelos.

fim(C1,C2):-(C1==[]->(C2==[]->true;false);false). %true -> fim do jogo.
%fase([],[]):-false. %fase 2


%jogar
vizinhoscolor(X,Y,Tab,Color):-vizinhoscolor(X,Y,Tab,Color,1).
vizinhoscolor(X,Y,Tab,Cam,Viz):-(Viz>8->false;!,(vizinhoex(Viz,X,Y,Tab,Cam)->true;N is Viz+1,!,vizinhoscolor(X,Y,Tab,Cam,N))).

%vizinhoex(Viz,X,Y,Tab,Valor):-(vizinho(Viz,X,Y,Tab,V)->!,write('\nvalor '),write(Viz),write(': '),write(V),(V==Valor->true;!,false);false). %for debug only
vizinhoex(Viz,X,Y,Tab,Valor):-(vizinho(Viz,X,Y,Tab,V)->!,(V==Valor->true;!,false);false).

vizinhosempty(X,Y,Tab):-vizinhosempty(X,Y,Tab,1).
vizinhosempty(X,Y,Tab,Viz):-(Viz>8->true;!,(vizinhoex(Viz,X,Y,Tab,e)->N is Viz+1,!,vizinhosempty(X,Y,Tab,N);(vizinhoex(Viz,X,Y,Tab,w)->!,N is Viz+1,!,vizinhosempty(X,Y,Tab,N);false))).

vizinhovalidator(X,Y,Tab,Valor):-vizinhovalidator(X,Y,Tab,Valor,1).
vizinhovalidator(X,Y,Tab,Valor,Viz):-(Viz>8->false;(vizinho(Viz,X,Y,Tab,Valor)->true;N is Viz+1,vizinhovalidator(X,Y,Tab,Valor,N))).

jogadavalida(1,X,Y,Tabuleiro):-getpos(X,Y,V,Tabuleiro),!,(V==e->(vizinhosempty(X,Y,Tabuleiro)->true;!,false);false).
jogadavalida(2,X,Y,Color,Tabuleiro):-getpos(X,Y,V,Tabuleiro),!,
(V==e->(vizinhoscolor(X,Y,Tabuleiro,Color)->mesmacor(Color,Mcolor),
(vizinhoscolor(X,Y,Tabuleiro,Mcolor)->false;true);!,false);false).

jogar(1,Camelo,X,Y,ListaLideres,NovaListaLideres):-
getTab(Tabuleiro),
(member(Camelo,ListaLideres)->(jogadavalida(1,X,Y,Tabuleiro)->setpos(X,Y,Camelo,Tabuleiro,NovoTab),
!,takeout(Camelo,ListaLideres,NovaListaLideres),
retract(getTab(Tabuleiro)),
assert(getTab(NovoTab));false);false).

jogar(2,Camelo,X,Y,ListaCam,NovaCam):-
getTab(Tabuleiro),
(member(Camelo,ListaCam)->(jogadavalida(2,X,Y,Camelo,Tabuleiro)->setpos(X,Y,Camelo,Tabuleiro,NovoTab),
!,takeout(Camelo,ListaCam,NovaCam),
retract(getTab(Tabuleiro)),
assert(getTab(NovoTab));false);false).

%I/O
printlist([]).
printlist([H|T]):-printlist(H),write(' '),printlist(T).
printlist(Elem):-write(Elem).

cin(X):-read(T),(integer(T)->X=T,true;cin(X)).

askpos(X,Y):-
write('\ncoluna:'),
cin(Xt),
write('Linha:'),
cin(Yt),
getTab(B),
(jogadavalida(1,Xt,Yt,B)->X=Xt,Y=Yt,true;write('jogada invalida\n'),askpos(X,Y)).

askpos(X,Y,C):-
write('\ncoluna:'),
cin(Xt),
write('Linha:'),
cin(Yt),
getTab(B),
(jogadavalida(2,Xt,Yt,C,B)->X=Xt,Y=Yt,true;write('jogada invalida\n'),askpos(X,Y,C)).


askLider(J,C):-
getL(J,L),
write('\nLideres disponiveis: '),
printlist(L),
write('\nLider: '),!,
cin(X),!,
(member(X,L)->C=X,true;write('camelo invalido\n'),askLider(J,C),true).

askCam(J,C):-
getC(J,L),
write('\nCamelos disponiveis: '),
printlist(L),
write('\nCamelo: '),!,
cin(X),!,
(member(X,L)->C=X,true;write('camelo invalido\n'),askCam(J,C),true).


fase1(J):-
getL(J,L1),!,
write('\nJogador '),write(J),write(': '),!,
askpos(X1,Y1),!,
askLider(J,Lsel1),!,
(jogar(1,Lsel1,X1,Y1,L1,L1N)->
retract(getL(J,L1)),
assert(getL(J,L1N));fase1(J)).

fase1:-
getTab(Tabuleiro),!,
printboard(Tabuleiro),!,
fase1(1),!,
getTab(NovoTab),!,
printboard(NovoTab),!,
fase1(2).


fase2(J):-
getC(J,C1),!,
write('\nJogador '),write(J),write(': '),!,
askCam(J,Csel1),!,
askpos(X1,Y1,Csel1),!,
(jogar(2,Csel1,X1,Y1,C1,C1N)->
retract(getC(J,C1)),
assert(getC(J,C1N));fase2(J)).

fase2:-
getTab(Tabuleiro),!,
printboard(Tabuleiro),!,
fase2(1),!,
getTab(NovoTab),!,
printboard(NovoTab),!,
fase2(2).


:-dynamic t/1.
test2:-retract(t(1)),assert(t(2)).
test(X):-assert(t(1)),test2,t(X).

fase2cheat:-retractall(getTab(_)),retractall(getL(_,_)),retractall(getC(_,_)),fase2tab(A), assert(getTab(A)),initplayer(1),initplayer(2),retractall(getL(_,_)),assert(getL(1,[])),assert(getL(2,[])),!,run.

gamestart:-retractall(getTab(_)),retractall(getL(_,_)),retractall(getC(_,_)),initialBoard(A), assert(getTab(A)),initplayer(1),initplayer(2),!,run.
%main rotine
run :-
(fase->fase1;fase2),!,
run.


