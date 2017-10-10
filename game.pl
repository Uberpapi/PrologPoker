
:-module(game,[echo/0, go/0, allin/0]).
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
  Newhandsplayed is Handsplayed + 1,%Increase handsplayed so we know when to raise the blinds
  IncBlind = Newhandsplayed mod 3,%When we got a multiple of 3 we want to raise the blinds
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
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  (B1 > B2 -> X = B1
  ; X = B2),
  (Stack - X > 0 -> Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You bet ~d$!', [X]), ai_magic(bet)
  ;Y is 0, Z is Pot + Stack + Stack, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You dont have sufficient stack, so you went allin!~nThe AI called!~n', []), allin),
  pt,
  nl.


raise :-
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  ( B1 > B2 -> X = B1
    ; X = B2),
  (Stack - X > 0, length(Deck, 48)-> S is Stack - X*1.5, P is Pot + X*1.5, setPokertable([S, P, [B1, B2], ai, Handsplayed]), ai_magic(raise), pt
  ; Stack - X > 0 -> Y is Stack - X*2, Z is Pot + X*2, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You raise ~d$!', [X]), nl,  ai_magic(raise),pt
  ; Y is 0, Z is Pot + Stack + Stack, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You dont have sufficient stack, so you went allin!~nThe AI called!~n', []), allin).

call :-
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], Last_to_Act, Handsplayed]),
  (B1 > B2 -> X = B1, W = player
    ; X = B2, W = ai),
  (   Stack - X < 0 -> Y is 0, Z is Pot + Stack + Stack, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You dont have sufficient stack, so you went allin!~nThe AI called!~n', []), allin
    ; length(Deck, 48), Last_to_Act == player -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), dealflop(Deck), write('Flop is: '), flop, nl, pt
    ; length(Deck, 48), Last_to_Act == ai -> write('You call!'), A = 5, S is Stack - X/2, P is Pot + X/2, setPokertable([S, P, [B1, B2], Last_to_Act, Handsplayed]), ai_magic(check), pt
    ; length(Deck, 44) -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), dealturn(Deck), write('Turn is: '), turn, nl, pt
    ; length(Deck, 42) -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), dealriver(Deck), write('River is: '), river, nl, pt
    ; length(Deck, 40) -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2),pt),
  (  length(Deck, 40) -> !
    ;  W == player, A \== 5, Stack - X > 0 -> ai_magic(check)
    ; A \== 5, Stack - X > 0 -> nl, write('Do you want to check, bet or fold?'), nl
    ; !).

allin :-
  deck(Deck),
  (\+flop(_) ->  dealflop(Deck)),
  (\+turn(_) -> deck(Deckafterflop), dealturn(Deckafterflop)),
  (\+river(_) -> deck(Deckafterturn), dealriver(Deckafterturn)),
  player1Cards(P1),
  player2Cards(P2),
  pt,
  whoWon(P1,P2).

fold :-
  write('You lost the hand'), nl,
  pt, nl,
  write('Write go to deal the next hand'), nl.
