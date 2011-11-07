:-dynamic getTab/1.

:-dynamic getL/2.
:-dynamic getC/2.

%gerar pontos para os poços...
getXpDefault([4,5,8,7,4,5,8,6,2,3,4,3,6,7,5,2,3,3]).
getYpDefault([2,3,4,5,6,5,7,20,21,19,13,14,17,14,21,12,14,15,20]).
getPDef([10,10,10,10,20,20,20,20,20,10,30,30,30,10,30]).
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

generate(X,Y,L,I,K,Tab):- I == 0 -> append(L,[],K), !;
                  choose(X,Z), choose(Y,M),(getpos(Z,M,e,Tab)->!,appender(Z,M,T), (not(member(T,L))->append([T],L,NovaL), delete2(Z,X,NovaX), delete2(M,Y,NovaY),I2 is I-1, generate(NovaX,NovaY,NovaL,I2,K,Tab);generate(X,Y,L,I,K,Tab));generate(X,Y,L,I,K,Tab)).

getpoints(L):-getXpDefault(X),getYpDefault(Y),getPDef(P),generatePalmPlaces(X,Y,P,[],L).
%chamada de teste generatePalmPlaces([1,2,3,4,13,14],[5,6,7,8,9,10],[10,20,10,40],[],K).
generatePalmPlaces(X,Y,P,L,K):- initialBoard(B),
                          generate(X,Y,L,15,K,B),!.


addPoints(PosPont,PL,TB,G):-choose(PosPont,[X|[Y|_]]),!,choose(PL,P),!,getpos(X,Y,Ret,TB),!,appender(X,Y,Posdel),!,delete2(Posdel,PosPont,NPosPont),(Ret==e->!,delete2(P,PL,NPL),!,setpos(X,Y,P,TB,NTB),addPoints(NPosPont,NPL,NTB,G);write('X '+X+' Y '+Y+' ret '+Ret+'Posiçao cheia\n'),Y1 is Y-1,X1 is X-1, appender(X1,Y1,Posadd),append(PosPont,[Posadd],Npos),addPoints(Npos,PL,TB,G)).
addPoints([],[],T,T).
initBoard(B):-initialBoard(TB),getpoints(L),getPDef(PL),addPoints(L,PL,TB,B),!,true.
initBoard(_).

estadofim(
[[0, w, w, w, w, w, w, w, w, w, w, w],
 [1, w, w, w, w, w, w, w, w, w, w, w],
 [2, w, w, e, e, e, w, w, e, e, w, w],
 [3, w, w, e, e, e, e, e, 1, e, e, w],
 [4, w, e, e, 20, e, e, e, e, e, e, w],
 [5, w, w, e, e, e, 20, e, e, e, e, w],
 [6, w, e, e, e, 10, e, e, 30, e, e, w],
 [7, w, e, e, e, e, 10, e, e, e, e, w],
 [8, w, e, 30, e, e, e, e, e, e, e, w],
 [9, w, e, e, e, e, e, e, 5, e, e, w],
 [10, w, e, 9, e, e, e, e, e, e, e, w],
 [11, w, 9, e, 9, e, 2, e, e, e, e, w],
 [12, w, 9, 9, 9, e, e, e, e, e, e, w],
 [13, w, e, e, 9, e, e, e, e, 30, e, w],
 [14, w, e, 9, w, e, e, e, e, 10, e, w],
 [15, w, e, 9, 20, w, e, 10, e, e, e, w],
 [16, w, 9, 9, w, w, w, e, e, e, 6, w],
 [17, w, e, 4, e, w, w, e, e, e, e, w],
 [18, w, e, e, e, 20, e, e, e, e, w, w],
 [19, w, e, e, e, e, e, e, 3, e, e, w],
 [20, w, e, e, e, e, e, e, e, e, w, w],
 [21, w, w, 10, 20, e, e, e, e, e, w, w],
 [22, w, e, e, 30, e, e, 10, e, w, w, w],
 [23, w, w, e, 8, 8, e, e, e, w, w, w],
 [24, w, w, 7, 8, 8, 0, e, w, w, w, w],
 [25, w, w, w, 8, 8, 8, 8, w, w, w, w],
 [26, w, w, w, 8, 8, 8, w, w, w, w, w],
 [27, w, w, w, w, e, e, w, w, w, w, w],
 [28, w, w, w, w, w, w, w, w, w, w, w],
 [29, w, w, w, w, w, w, w, w, w, w, w]]).

fase2tab([
[0, w, w, w, w, w, w, w, w, w, w, w],
[0, w, w, w, w, w, w, w, w, w, w, w],
 [1, w, w, e, 1, e, w, w, e, e, w, w],
 [2, w, w, e, e, e, e, 5, e, e, e, w],
 [3, w, e, e, e, 3, e, e, e, e, e, w],
 [4, w, w, e, e, e, e, e, e, e, e, w],
 [5, w, e, e, e, e, 0, e, e, e, e, w],
 [6, w, e, e, e, e, e, e, e, e, e, w],
 [7, w, e, e, 2, e, e, e, e, e, e, w],
 [8, w, e, e, e, e, e, e, e, e, e, w],
 [9, w, e, e, e, e, e, e, e, 6, e, w],
 [10, w, e, e, e, e, e, e, e, e, e, w],
 [11, w, e, e, e, e, e, e, e, e, e, w],
 [12, w, e, e, 7, e, e, e, e, e, e, w],
 [13, w, e, e, w, e, e, e, e, e, e, w],
 [14, w, e, e, e, w, e, e, e, e, e, w],
 [15, w, e, e, w, w, w, e, e, e, e, w],
 [16, w, e, 4, e, w, w, e, e, e, e, w],
 [17, w, e, e, e, e, e, e, e, e, w, w],
 [18, w, e, e, e, e, e, e, e, 10, e, w],
 [19, w, e, e, e, e, e, e, e, e, w, w],
 [20, w, w, e, e, e, e, e, e, 9, w, w],
 [21, w, e, e, e, e, e, 8, e, w, w, w],
 [22, w, w, e, e, e, e, e, e, w, w, w],
 [23, w, w, e, e, e, e, e, w, w, w, w],
 [24, w, w, w, e, e, e, e, w, w, w, w],
 [25, w, w, w, e, e, e, w, w, w, w, w],
 [26, w, w, w, w, e, e, w, w, w, w, w],
 [27, w, w, w, w, w, w, w, w, w, w, w],
 [29, w, w, w, w, w, w, w, w, w, w, w]]).
 
p1c([1,3,5,7,9]).
p2c([0,2,4,6,8]).

initialBoard(
[[0,w,w,w,w,w,w,w,w,w,w,w],
[1,w,w,w,w,w,w,w,w,w,w,w],
[2,w,w,e,e,e,w,w,e,e,w,w],
[3,w,w,e,50,e,e,e,e,e,e,w],
[4,w,e,e,e,e,e,e,e,e,e,w],
[5,w,w,e,e,e,e,e,e,e,e,w],
[6,w,e,e,e,e,e,e,e,e,e,w],
[7,w,e,e,e,e,e,e,e,e,e,w],
[8,w,e,e,e,e,e,e,e,50,e,w],
[9,w,e,e,e,e,e,e,e,e,e,w],
[10,w,e,e,e,e,e,e,e,e,e,w],
[11,w,e,e,e,e,e,e,e,e,e,w],
[12,w,e,50,e,e,e,e,e,e,e,w],
[13,w,e,e,e,e,e,e,e,e,e,w],
[14,w,e,e,w,e,e,e,e,e,e,w],
[15,w,e,e,e,w,e,e,e,e,e,w],
[16,w,e,e,w,w,w,e,e,e,e,w],
[17,w,e,e,e,w,w,e,50,e,e,w],
[18,w,e,e,e,e,e,e,e,e,w,w],
[19,w,e,e,e,e,e,e,e,e,e,w],
[20,w,e,e,e,e,e,e,e,e,w,w],
[21,w,w,e,e,e,e,e,e,e,w,w],
[22,w,e,e,e,50,e,e,e,w,w,w],
[23,w,w,e,e,e,e,e,e,w,w,w],
[24,w,w,e,e,e,e,e,w,w,w,w],
[25,w,w,w,e,e,e,e,w,w,w,w],
[26,w,w,w,e,e,e,w,w,w,w,w],
[27,w,w,w,w,e,e,w,w,w,w,w],
[28,w,w,w,w,w,w,w,w,w,w,w],
[29,w,w,w,w,w,w,w,w,w,w,w]]
).

enclosed(
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
[16,w,e,e,e,w,w,4,4,4,4,w],
[17,w,e,e,e,e,4,4,4,4,w,w],
[18,w,e,e,e,e,4,4,e,e,e,w],
[19,w,e,e,e,e,4,4,e,e,w,w],
[20,w,w,e,e,e,4,4,e,e,w,w],
[21,w,3,3,3,3,3,3,3,w,w,w],
[22,w,w,3,3,50,3,3,3,w,w,w],
[23,w,w,e,30,e,e,e,w,w,w,w],
[24,w,w,w,e,e,e,e,w,w,w,w],
[25,w,w,w,e,50,e,w,w,w,w,w],
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
rowanalise([Head|Tail]):-N is Head/2,write('   '), (integer(N) -> write(' /'); write(' \\ /')), printwhite(Tail),write('\n'),(Head<10->write(Head),write('  ');write(Head),write(' ')),(integer(N) -> write('| '); write('  | ')),printrow(Tail).
printwhite([]).
printwhite([_|T]):-write(' \\ /'),printwhite(T).
printrow([]).
printrow([Head|Tail]):-(Head=e->write(' ');printelem(Head)), write(' | '),printrow(Tail).
printelem(H):-(integer(H)->(H>9,H<40->write('P');(H>40->write('S');write(H)));write(H)).


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
initplayer(1):-initplayer(L,C,P,1),assert(getL(1,L)),assert(getC(1,C)),assert(getP(1,P)).
initplayer(L,C,P,1):-L=[1,3,5,7,9],createnewhand(11,[],C),P=[].
%inicializador do jogador 2 joga com as pecas pares.
initplayer(L,C,P,2):-L=[0,2,4,6,8],createnewhand(10,[],C),P=[].
getPlayer(X,2):-N is X/2,(integer(N)->true;false).
getPlayer(X,1):-not(getPlayer(X,2)).


createnewhand(N,LC,D):-N1 is N-2,(N1>=0->addcamel(10,N1,LC,R),createnewhand(N1,R,D);D=LC).

%gamestate
fase:- getL(1,L1),getL(2,L2),(L1==[]->(L2==[]->false;true);true). %true -> fase de colocacao de lideres %false -> fase de colocacao de camelos.
allmembers:-p1c(A),p2c(B),allmembers(1,A),allmembers(2,B).
allmembers(J,[H|T]):-getC(J,L),(member(H,L)->allmembers(J,T);false).
allmembers(_,[]).
fim:-allmembers. %true -> fim do jogo.
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
jogadavalida(2,X,Y,Color,Tabuleiro):-getpos(X,Y,V,Tabuleiro),!,(integer(V)->((V>9,V<40)->V1=e;V1=V);V1=V),
(V1==e->(vizinhoscolor(X,Y,Tabuleiro,Color)->mesmacor(Color,Mcolor),
(vizinhoscolor(X,Y,Tabuleiro,Mcolor)->false;true);!,false);false).

jogar(1,Camelo,X,Y,ListaLideres,NovaListaLideres):-
getTab(Tabuleiro),
(member(Camelo,ListaLideres)->(jogadavalida(1,X,Y,Tabuleiro)->setpos(X,Y,Camelo,Tabuleiro,NovoTab),
!,takeout(Camelo,ListaLideres,NovaListaLideres),
retract(getTab(Tabuleiro)),
assert(getTab(NovoTab));false);false).

jogar(2,Camelo,X,Y,ListaCam,NovaCam):-
getTab(Tabuleiro),
(member(Camelo,ListaCam)->(jogadavalida(2,X,Y,Camelo,Tabuleiro)->getpos(X,Y,V,Tabuleiro),
        !,(integer(V)->!,(even(Camelo)->J=2;J=1),!,getP(J,Pont),!,
        Pcalc is V//10,!,
        append(Pont,[Pcalc],PN),!,
        retract(getP(J,Pont)),!,
        assert(getP(J,PN)),true;true),!,
setpos(X,Y,Camelo,Tabuleiro,NovoTab),
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
getP(J,Pont),!,
write('\nJogador '),write(J),write(': '),!,
write('\nPecas de Pontos '),!,printlist(Pont),!,
askCam(J,Csel1),!,
askpos(X1,Y1,Csel1),!,
(jogar(2,Csel1,X1,Y1,C1,C1N)->
retract(getC(J,C1)),
assert(getC(J,C1N));fase2(J)).

fase2:-
getTab(Tabuleiro),!,
printboard(Tabuleiro),!,
fase2(1),!,
getTab(NovoTab1),!,
printboard(NovoTab1),!,
fase2(1),!,
getTab(NovoTab2),!,
printboard(NovoTab2),!,
fase2(2),!,
getTab(NovoTab3),!,
printboard(NovoTab3),!,
fase2(2).


:-dynamic t/1.
test2:-retract(t(1)),assert(t(2)).
test(X):-assert(t(1)),test2,t(X).

fimcheat:-retractall(done),retractall(getTab(_)),retractall(getL(_,_)),retractall(getC(_,_)),retractall(getP(_,_)),estadofim(A), assert(getTab(A)),initplayer(1),initplayer(2),retractall(getL(_,_)),assert(getL(1,[])),assert(getL(2,[])),retractall(getC(_,_)),assert(getC(1,[])),assert(getC(2,[])),!,run.
fase2cheat:-retractall(done),retractall(getTab(_)),retractall(getL(_,_)),retractall(getC(_,_)),retractall(getP(_,_)),fase2tab(A), assert(getTab(A)),initplayer(1),initplayer(2),retractall(getL(_,_)),assert(getL(1,[])),assert(getL(2,[])),!,run.

show_lastpontuation:-getTab(B),caravanpoints,findalle(0,0,B,N),printboard(N),nl,getPoint(1,B),nl,getPoint(2,B),assert(done).
getpalmeirasfor(_,[],_).
getpalmeirasfor(J,[H|T],Tab):-shearchnear(H,Tab,_,0,0,0),getpalmeirasfor(J,T,Tab).

countelemlist(L,N,Elem):-countelemlist(L,N,Elem,0).

countelemlist([],A,_,B):-A=B.
countelemlist([H|T],N,Elem,Acc):-(H==Elem->Acc1 is Acc+1,countelemlist(T,N,Elem,Acc1);countelemlist(T,N,Elem,Acc)).
caravan([]):-true.
caravan([H|T]):-getC(1,C1),getC(2,C2),countelemlist(C1,N1,H),mesmacor(H,H1),countelemlist(C2,N2,H1),
(N1>N2->getP(1,P1),retractall(getP(1,_)),append(P1,[10],NP1),assert(getP(1,NP1));
(N2>N1->getP(2,P2),retractall(getP(2,_)),append(P2,[10],NP2),assert(getP(2,NP2));
(N2==N1->getP(1,P1),retractall(getP(1,_)),append(P1,[5],NP1),assert(getP(1,NP1)),getP(2,P2),retractall(getP(2,_)),append(P2,[5],NP2),assert(getP(2,NP2))
;false))),caravan(T).

caravanpoints:-p1c(X),caravan(X).

getpalmeirasfor(J,Tab):-(J==1->p1c(Lista);p2c(Lista)),!,getpalmeirasfor(J,Lista,Tab).
getPoint(J,N):-(J==1->write('Jogador 1:\n'),
	countPec(a,N,Times),(shearchnear(a,N,T,0,0,0)->true;true),getpalmeirasfor(1,T),getP(1,K),sumList(K,Res),write('Pontuacao: '),!,write(Res);
	write('Jogador 2:\n'),countPec(z,N,Times),(shearchnear(z,N,T,0,0,0)->true;true),getpalmeirasfor(2,T),getP(2,K),sumList(K,Res),write(Res)).
	
gamestart:-retractall(done),retractall(getTab(_)),retractall(getL(_,_)),retractall(getC(_,_)),retractall(getP(_,_)),initBoard(A), assert(getTab(A)),initplayer(1),initplayer(2),!,run.
%main rotine
run :-
(not(done)->
(fase->fase1;fim->fase2;show_lastpontuation),!,
run;true).

floodfill3(X,Y,Target_key,Replace_key, Tab,NT):-
(getpos(X,Y,V,Tab)->(not(V==Target_key)->
(integer(V),V>9,(Target_key==a;not(even(Target_key)),V>40)->((Target_key==a;not(even(Target_key)))->J=1;J=2),(getP(J,L)->L1 =L;L1 =[]),V1 is V/10,append(L1,[V1],NLP),retractall(getP(J,_)),assert(getP(J,NLP)),setpos(X,Y,Target_key,Tab,NTab),floodfill3(X,Y,Target_key,Replace_key, NTab,NT),printboard(NT);NT=Tab),
true;
setpos(X,Y,Replace_key,Tab,NTab),!,
Y1 is Y-2,!, floodfill3(X,Y1,Target_key,Replace_key,NTab,NT1),!,
Y2 is Y+2,!, floodfill3(X,Y2,Target_key,Replace_key,NT1,NT2),!,
Y3 is Y-1,!,(even(Y)->X1 is X-1;X1 is X),!, floodfill3(X1,Y3,Target_key,Replace_key,NT2,NT3),!,
Y4 is Y-1,!,(even(Y)->X2 is X;X2 is X+1),!, floodfill3(X2,Y4,Target_key,Replace_key,NT3,NT4),!,
Y5 is Y+1,!,(even(Y)->X3 is X-1;X3 is X),!, floodfill3(X3,Y5,Target_key,Replace_key,NT4,NT5),!,
Y6 is Y+1,!,(even(Y)->X4 is X;X4 is X+1),!, floodfill3(X4,Y6,Target_key,Replace_key,NT5,NT6),!,
X5 is X-1,!,floodfill3(X5,Y,Target_key,Replace_key,NT6,NT7),!,
X6 is X+1,!, floodfill3(X6,Y,Target_key,Replace_key,NT7,NT));write('fim'),NT=Tab).


floodfill2(X,Y,Target_key,Replace_key, NReplace,Tab,NT):-
notrace,!,(getpos(X,Y,V,Tab)->notrace,(not(V==Target_key)->(V==w->NT=Tab;((V==Replace_key;V==NReplace)->NT=Tab;(integer(V),V>9->NT=Tab;false))),true;
notrace,setpos(X,Y, NReplace,Tab,NTab),!,notrace,
Y1 is Y-2,!, floodfill2(X,Y1,Target_key,Replace_key, NReplace,NTab,NT1),!,
Y2 is Y+2,!, floodfill2(X,Y2,Target_key,Replace_key, NReplace,NT1,NT2),!,
Y3 is Y-1,!,(even(Y)->X1 is X-1;X1 is X),!, floodfill2(X1,Y3,Target_key,Replace_key, NReplace,NT2,NT3),!,
Y4 is Y-1,!,(even(Y)->X2 is X;X2 is X+1),!, floodfill2(X2,Y4,Target_key,Replace_key, NReplace,NT3,NT4),!,
Y5 is Y+1,!,(even(Y)->X3 is X-1;X3 is X),!, floodfill2(X3,Y5,Target_key,Replace_key, NReplace,NT4,NT5),!,
Y6 is Y+1,!,(even(Y)->X4 is X;X4 is X+1),!, floodfill2(X4,Y6,Target_key,Replace_key, NReplace,NT5,NT6),!,
X5 is X-1,!,floodfill2(X5,Y,Target_key,Replace_key, NReplace,NT6,NT7),!,
X6 is X+1,!, floodfill2(X6,Y,Target_key,Replace_key, NReplace,NT7,NT));notrace,NT=Tab).


floodfill(X,Y,Target_key,Replace_key, Tab,NT):-
notrace,(getpos(X,Y,V,Tab)->notrace,(not(V==Target_key)->NT=Tab,true;
notrace,setpos(X,Y,Replace_key,Tab,NTab),!,notrace,
Y1 is Y-2,!, floodfill(X,Y1,Target_key,Replace_key,NTab,NT1),!,
Y2 is Y+2,!, floodfill(X,Y2,Target_key,Replace_key,NT1,NT2),!,
Y3 is Y-1,!,(even(Y)->X1 is X-1;X1 is X),!, floodfill(X1,Y3,Target_key,Replace_key,NT2,NT3),!,
Y4 is Y-1,!,(even(Y)->X2 is X;X2 is X+1),!, floodfill(X2,Y4,Target_key,Replace_key,NT3,NT4),!,
Y5 is Y+1,!,(even(Y)->X3 is X-1;X3 is X),!, floodfill(X3,Y5,Target_key,Replace_key,NT4,NT5),!,
Y6 is Y+1,!,(even(Y)->X4 is X;X4 is X+1),!, floodfill(X4,Y6,Target_key,Replace_key,NT5,NT6),!,
X5 is X-1,!,floodfill(X5,Y,Target_key,Replace_key,NT6,NT7),!,
X6 is X+1,!, floodfill(X6,Y,Target_key,Replace_key,NT7,NT));notrace,NT=Tab).


allintegersequal([H|T],C,Ret):-
((integer(H),H<10)->
        (C==e->
                !,allintegersequal(T,H,Ret),!
                ;
                        (C==H->
                                !,allintegersequal(T,H,Ret),!,
                                true
                )
        )
        ;
        !,allintegersequal(T,C,Ret),!
).

allintegersequal([],T,Ret):-(integer(T)->Ret=T;false).

getneighboorint(X,Y,V,B):-
write('X: '),write(X),write(' Y: '),write(Y),nl,
(vizinho(1,X,Y,B,Valor)->true;true,Valor=e),!,
(vizinho(2,X,Y,B,Valor1)->true;true,Valor1=e),!,
(vizinho(3,X,Y,B,Valor2)->true;true,Valor2=e),!,
(vizinho(4,X,Y,B,Valor3)->true;true,Valor3=e),!,
(vizinho(5,X,Y,B,Valor4)->true;true,Valor4=e),!,
(vizinho(6,X,Y,B,Valor5)->true;true,Valor5=e),!,
(vizinho(7,X,Y,B,Valor6)->true;true,Valor6=e),!,
(vizinho(8,X,Y,B,Valor7)->true;true,Valor7=e),!,
LV=[Valor,Valor1,Valor2,Valor3,Valor4,Valor5,Valor6,Valor7],printlist(LV),nl,!,
(allintegersequal(LV,e,Ret)->write('valid'),write('V: '),write(V),nl,V=Ret,true;false).


findalle(Tab,FT):-findalle(0,0,Tab,FT).

findalle(X,Y,Tab,FT):-
%write('X: '),write(X),write(' Y: '),write(Y),
(getpos(X,Y,V,Tab)->!,retractall(error),!,
        write('V: '),write(V),nl,!,
        (V==e->
                (getneighboorint(X,Y,CAR,Tab)->
                        !,(even(CAR)->J=z;J=a),(floodfill2(X,Y,e,CAR,J,Tab,Lodo)->NT=Lodo;floodfill(X,Y,e,' ',Tab,NT)),notrace
                        ;
                        !,floodfill(X,Y,e,' ',Tab,NT)
                        ),!,X2 is X+1,!,findalle(X2,Y,NT,FT)
                ;

                true,!,X1 is X+1,!,findalle(X1,Y,Tab,FT))
        ;
        (error->write('done'),FT=Tab,true;assert(error),X2=0,Y2 is Y+1,!,findalle(X2,Y2,Tab,FT))
).

sumList(L,R):-sumList(L,R,0).
sumList([Head|Tail],Res,Acc):-Res1 is Head+Acc,sumList(Tail,Res,Res1).
sumList([],Res,Acc):-Res=Acc.


% test enclosed - enclosed(B),findalle(0,0,B,N),printboard(N),countPec(a,N,Times),(shearchnear(a,N,T,0,0,0)->true;true),getP(1,K),sumList(K,Res).
shearchnear(Peca,Tab,N,X,Y,Acc):-
(getpos(X,Y,V,Tab)->!,retractall(error),!,(V==Peca->(floodfill3(X,Y,Peca,'r',Tab,FG)->true;true),write('lodo'),printboard(FG),N=FG;X2 is X+1,!,shearchnear(Peca,Tab,N,X2,Y,Acc));
(error->write('done'),N=Acc,true;assert(error),X2=0,Y2 is Y+1,!,shearchnear(Peca,Tab,N,X2,Y2,Acc))).


countPec(Peca,Tab,N):-countPecaux(Peca,Tab,N,0,0,0).
countPecaux(Peca,Tab,N,X,Y,Acc):-
(getpos(X,Y,V,Tab)->!,retractall(error),!,(V==Peca->Acc1 is Acc+1,X2 is X+1,!,countPecaux(Peca,Tab,N,X2,Y,Acc1);X2 is X+1,!,countPecaux(Peca,Tab,N,X2,Y,Acc));
(error->write('done'),N=Acc,true;assert(error),X2=0,Y2 is Y+1,!,countPecaux(Peca,Tab,N,X2,Y2,Acc))).