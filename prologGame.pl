findCard([H|_],1,H).
findCard([_|T],N,Card):- New is N-1,findCard(T,New,Card). 

deleteCard([_|T],1,T).
deleteCard([H|T],N,[H|A]):- New is N-1,deleteCard(T,New,A).

printCard(Card):- 
	getCard(Card,Att,Def,Mode),
	write(' Card(Att '),write(Att),
	write('| Def '),write(Def),
	write('| Mode '),write(Mode),
	writeln(')').
printCardList([],_).
printCardList([H|T],Num):-
	write(Num),write(')'),printCard(H),N is Num + 1,printCardList(T,N).

numberOfCardAux([],Sum,Sum).
numberOfCardAux([_|T],Accum,Sum):- NewAccum is Accum + 1, numberOfCardAux(T,NewAccum,Sum).
numberOfCard(L,Sum):-numberOfCardAux(L,0,Sum).

getBattleCard(P1,P2,Card1,Card2):-
	onTable(1,List1),onTable(2,List2),
	findCard(List1,P1,Card1),findCard(List2,P2,Card2).
getCard((Att,Def,Mode),Att,Def,Mode).
setMode((Att,Def,_),Mode,(Att,Def,Mode)).

genCard(Card):- random(1,11,A),random(1,11,D),Card = (A,D,-1),!.
createDeck(0,[]).
createDeck(N,[H|T]):- P is N-1,genCard(H),createDeck(P,T),!. 
storeDeck(P,Deck):- asserta(deck(P,Deck)).

how():-
	writeln("--------------List of function (Can call by 'how()')---------------"),
	writeln("drawCard(Player)"),
	writeln("listCardOnHand(Player)"),
	writeln("summon(Player, NumberOfCard, Mode(0 = Def,1 = Att))"),
	writeln("listCardOnTable(Player)"),
	writeln("attack(PlayerWhoCall, NumberOfPlayer1Card, NumberOfPlayer2Card)"),
	writeln("turnChange()"),
	writeln("-------------------------------------------------------------------").
	
play():-
	reconsult('prologGame.pl'),
	createDeck(40,Deck1),storeDeck(1,Deck1),
	createDeck(40,Deck2),storeDeck(2,Deck2),
	draw4(1),draw4(2),
	nl,run().

run():- isEnd().
run():-
	turn(T), write(">>>>>> Player "),writeln(T),nl,
	drawCard(T),writeln(">>> A card was drawn."),
	writeln("Card on hand:"),listCardOnHand(T),
	nl,writeln("Enter number of card to summon"),read(N),
	writeln("Enter mode to summon (0=Def,1=Att)"),read(M),
	summon(T,N,M),
	writeln("Card on hand:"),listCardOnTable(T),
	writeln("Enter number of your card to attack or '0' to skip"),read(N1),
	writeln("Enter number of enemy card to attack or '0' if it has no card"),read(N2),
	E is mod(T,2)+1,attack(T,E,N1,N2),
	turnChange(),run().
	
:- dynamic playerLife/2.
:- dynamic remainCard/2.
:- dynamic onHand/2.
:- dynamic onTable/2.
:- dynamic summonflag/1.
:- dynamic turn/1.
:- dynamic deck/2.

playerLife(1,20).
playerLife(2,20).

remainCard(1,40).
remainCard(2,40).

turn(1).

onHand(1,[]).
onHand(2,[]).

onTable(1,[]).
onTable(2,[]).

updatePlayer(Player,Point):- retract(playerLife(Player,P)),New_P is P+Point,asserta(playerLife(Player,New_P)).

draw4(Player):- draw4Aux(Player,0,4).
draw4Aux(_,N,N).
draw4Aux(Player,Acc,N):- New is Acc + 1,drawCard(Player),draw4Aux(Player,New,N).

drawCard(Player):- turn(I),Player \= I,writeln("Not your turn. Wait for turnChange()."),!.
drawCard(Player):- remainCard(Player,R),R==0,writeln("No more cards in deck!!!"),!.
drawCard(Player):-
	onHand(Player,C),numberOfCard(C,NumOnhand),NumOnhand < 5,
	deck(Player,D),remainCard(Player,R),findCard(D,R,Card),
	retract(remainCard(Player,N)), New is N-1,
	asserta(remainCard(Player,New)),
	retract(onHand(Player,L)), append(L,[Card],Result),
	asserta(onHand(Player,Result)),!.
drawCard(_):- writeln("You have 5 cards on hand.").

listCardOnHand(Player):- onHand(Player,Card),printCardList(Card,1).
listCardOnTable(Player):- onTable(Player,Card),printCardList(Card,1).

summon(Player,_,_):- 
	turn(I),Player \= I,writeln("Not your turn. Wait for turnChange()."),!.

summon(_,_,_):- 
	summonflag(_),writeln("Only one monster can be summoned per turn."),!.

summon(Player,_,_):- 
	onHand(Player, CardList), numberOfCard(CardList,N_OnHand), N_OnHand == 0,writeln("You don't have any cards on hand."),!.

summon(Player,_,_):- 
	onTable(Player, CardList), numberOfCard(CardList,N_OnTable), N_OnTable == 3,writeln("You have 3 cards on table."),!.

summon(Player,Card_Num,Mode):-
	retract(onHand(Player, CardList)),
	retract(onTable(Player,TableList)),
	findCard(CardList,Card_Num,Card_temp),setMode(Card_temp,Mode,Card),
	deleteCard(CardList,Card_Num,New_CardList),
	append(TableList,[Card],New_CardList_T),
	asserta(onHand(Player,New_CardList)),
	asserta(onTable(Player,New_CardList_T)),
	asserta(summonflag(Player)),
	write('Summon -> '),printCard(Card),!.

deleteOnTable(Player,N):-
	retract(onTable(Player,CardList)),
	deleteCard(CardList,N,New_CardList),
	asserta(onTable(Player,New_CardList)).

attack(Player,_):-
	turn(I),Player \= I,writeln("Not your turn. Wait for turnChange()."),!.

attack(Player,P):-
	onTable(Player,CardList),
	findCard(CardList,P,Card),
	getCard(Card,Att,_,Mode),
	Mode == 1,PT is mod(Player,2),Update is -1*Att,
	updatePlayer(1,Update),write("Player"),write(PT),writeln(" life point is decreased"),isEnd(),!.

attack(Player,_,_):-
	turn(I),Player \= I,writeln("Not your turn. Wait for turnChange()."),!.

attack(1,P1_Card,_):- 
	onTable(1,CardList),
	findCard(CardList,P1_Card,Card),
	getCard(Card,_,_,Mode),Mode == 0,
	writeln("This card is in defence mode.!!!"),!.
attack(1,P1_Card,P2_Card):- 
	getBattleCard(P1_Card,P2_Card,Card1,Card2),
	getCard(Card1,Att1,_,_),getCard(Card2,_,Def2,Mode2),
	Mode2 == 0,Att1 >= Def2,deleteOnTable(2,P2_Card),write(Card2),writeln(" is destroyed."),!.
attack(1,P1_Card,P2_Card):- 
	getBattleCard(P1_Card,P2_Card,Card1,Card2),
	getCard(Card1,Att1,_,_),getCard(Card2,_,Def2,Mode2),
	Mode2 == 0,Att1 < Def2,Update is Att1-Def2,updatePlayer(1,Update),deleteOnTable(1,P1_Card),
	writeln("Player1 life point is decreased"),isEnd(),!.

attack(2,_,P2_Card):- 
	onTable(2,CardList),
	findCard(CardList,P2_Card,Card),
	getCard(Card,_,_,Mode),Mode == 0,
	writeln("This card is in defence mode.!!!"),!.
attack(2,P1_Card,P2_Card):- 
	getBattleCard(P1_Card,P2_Card,Card1,Card2),
	getCard(Card1,_,Def1,Mode1),getCard(Card2,Att2,_,_),
	Mode1 == 0,Att2 >= Def1,deleteOnTable(1,P1_Card),write(Card1),writeln(" is destroyed."),!.
attack(2,P1_Card,P2_Card):- 
	getBattleCard(P1_Card,P2_Card,Card1,Card2),
	getCard(Card1,_,Def1,Mode1),getCard(Card2,Att2,_,_),
	Mode1 == 0,Att2 < Def1,Update is Att2-Def1,updatePlayer(2,Update),deleteOnTable(2,P2_Card),
	writeln("Player2 life point is decreased"),isEnd(),!.

attack(_,P1_Card,P2_Card):-
	getBattleCard(P1_Card,P2_Card,Card1,Card2),
	getCard(Card1,Att1,_,_),getCard(Card2,Att2,_,_),
	Att1 > Att2,deleteOnTable(2,P2_Card),
	Update is Att2-Att1,updatePlayer(2,Update),
	writeln("Player2 life point is decreased. | "),write(Card2),writeln(" is destroyed."),isEnd(),!.
attack(_,P1_Card,P2_Card):-
	getBattleCard(P1_Card,P2_Card,Card1,Card2),
	getCard(Card1,Att1,_,_),getCard(Card2,Att2,_,_),
	Att1 < Att2,deleteOnTable(1,P1_Card),
	Update is Att1-Att2,updatePlayer(1,Update),
	writeln("Player1 life point is decreased. | "),write(Card1),writeln(" is destroyed."),isEnd(),!.

turnChange():- 
	retract(summonflag(_)),
	retract(turn(T)),K is mod(T,2)+1,
	asserta(turn(K)),!.

turnChange():- 
	retract(turn(T)),K is mod(T,2)+1,
	asserta(turn(K)).

isEnd():- playerLife(Player,P),P =< 0,PT is mod(Player,2)+1,write("Player "),write(PT),writeln(" Win!!!!!!"),!.
