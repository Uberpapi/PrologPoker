
:-module(game,[echo/0, go/0]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(ai).
:-use_module(aiLogic).
:-use_module(printtable).

p1 :- player1(X), write(X).
p2 :- player2(X), write(X).
flop :- flop(X), write(X).
turn :- turn(X), write(X).
river :- river(X), write(X).

echo :-
  write('>> '),
  read(X),
  (X == end_of_file  -> nl, write('Exiting echo'),!
  ;accepted_commands(X) -> call(X), echo
  ;format('~w is not a valid command.~nThe valid commands are:~nplay - Restart the game~ngo   - Deals a new hand~nAnd all the poker actions~n', [X]), echo).

accepted_commands(X) :-
  (X == play; X== go; X == check; X == call; X == bet; X == raise; X == fold).

play :-
  retractall(flop(_)),
  retractall(turn(_)),
  retractall(river(_)),
  retractall(player1(_)),
  retractall(player2(_)),
  setPokertable([1000, 0, [5,10], ai, 0]),nl,
  pt,
  write('Hello and welcome to this uber good poker game'), nl,
  write('You start with a stack of 1000'), nl,
  write('The different commands is "check", "bet", "call" or "fold"'), nl.


go :-
  retractall(flop(_)),
  retractall(turn(_)),
  retractall(river(_)),
  pokertable([Stack, _, [B1,B2], Last_to_Act, Handsplayed]),% Check if anyone have won the game
 %Or else do everything as usual
  createDeck(X),
  dealtp(X),

  (B1 > B2 -> W = ai %Checks who starts to act
  ; W = player),
  Newhandsplayed = Handsplayed + 1,
  IncBlind = Newhandsplayed mod 3,
  (IncBlind = 0 -> NewB2 is B2*2, NewB1 is B1*2 %When IncBlind is 0 we want to double the blinds
  ; NewB2 is B2, NewB1 is B1),

  Aistack is 2000-Stack,
  (Last_to_Act = player ->
    (Stack > NewB2 -> Y is Stack-NewB2, Z is NewB1+NewB2
    ; Y = 0, Z = NewB1 + Stack) %If we cant afford the blind, take the whole stack as blind
  ; (Aistack > NewB1 ->  Y is Stack - NewB2, Z is NewB1+NewB2
    ;Y is Stack, Z is NewB1 + Stack)),%If the AI cant afford the blind, take its whole stack as blind
  setPokertable([Y, Z, [NewB2,NewB1], W, Newhandsplayed]),
  pt,
  ( W == player -> ai_magic(pre) ; write('Do you want to call, raise or fold?'), nl).

check :-
  deck(Deck),
  pokertable([_, _, _, Last_to_Act, _]),
  (   length(Deck, 48), Last_to_Act == player -> nl, dealflop(Deck), write('Flop is: '), flop, ai_magic(check)
    ; length(Deck, 44), Last_to_Act == player ->  nl, dealturn(Deck), write('Turn is: '), turn, ai_magic(check)
    ; length(Deck, 42), Last_to_Act == player ->  nl, dealriver(Deck), write('River is: '), river, ai_magic(check)
    ; length(Deck, 40), Last_to_Act == player -> nl, player1Cards(X), player2Cards(Y), whoWon(X,Y)
    ; Last_to_Act == ai -> nl, ai_magic(check)
    ),
    pt.

bet :-
  pokertable([Stack, Pot, [B1,B2], _, _]),

  (B1 > B2 -> X = B1
  ; X = B2),
  (Stack - X > 0 -> Y is Stack - X, Z is Pot + X, format('You bet ~d$!', [X])
  ;Y is 0, Z is Pot + Stack),
  nl,
  setPokertable([Y, Z, [B1, B2], ai, _]),
  ai_magic(bet),
  pt.

raise :-
  deck(Deck),
  write('You raise!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], _, _]),
  (   B1 > B2 -> X = B1
    ; X = B2),
  Y is Stack - X*2,
  Z is Pot + X*2,
  S is Stack - X*1.5,
  P is Pot + X*1.5,
  (length(Deck, 48) -> setPokertable([S, P, [B1, B2], ai, _])
  ; setPokertable([Y, Z, [B1, B2], ai, _])),
  ai_magic(raise),
  pt.

call :-
  deck(Deck),
  write('You call!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], Last_to_Act, _]),
  (B1 > B2 -> X = B1, W = player
    ; X = B2, W = ai),
  Y is Stack - X,
  Z is Pot + X,
  S is Stack - X/2,
  P is Pot + X/2,
  ( length(Deck, 48), Last_to_Act == player -> setPokertable([Y, Z, [B1, B2], W, _]), dealflop(Deck), write('Flop is: '), flop
    ; length(Deck, 48), Last_to_Act == ai -> A = 5, setPokertable([S, P, [B1, B2], Last_to_Act, _]), ai_magic(check)
    ; length(Deck, 44) -> setPokertable([Y, Z, [B1, B2], W, _]), dealturn(Deck), write('Turn is: '), turn
    ; length(Deck, 42) -> setPokertable([Y, Z, [B1, B2], W, _]), dealriver(Deck), write('River is: '), river
    ; length(Deck, 40) -> setPokertable([Y, Z, [B1, B2], W, _]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2)
    ),
  (  length(Deck, 40) -> !
    ;  W == player, A \== 5 -> ai_magic(check)
    ; A \== 5 -> nl, write('Do you want to check, bet or fold?')
    ; !),
  pt.

fold :-
  write('You lost the hand'),
  nl,
  pt.
