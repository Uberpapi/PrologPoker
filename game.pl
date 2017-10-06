
:-module(game,[echo/0, done/0, go/0]).
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
  call(X),
  echo.

play :-
  pt,
  setPokertable([1000, 0, [10,20], ai]),nl,
  write('Hello and welcome to this uber good poker game'), nl,
  write('You start with a stack of 1000'), nl,
  write('The different commands is "check", "bet", "call" or "fold"'), nl.


go :-
  retractall(flop(_)),
  retractall(turn(_)),
  retractall(river(_)),
  createDeck(X),
  dealtp(X),
  pokertable([Stack, _, [B1,B2], _]),
  Y is Stack - B2,
  Z is B1+B2,
  (B1 > B2 -> W = ai
  ; W = player),
  setPokertable([Y, Z, [B2,B1], W]),
  pt,
  ( W == player -> ai_magic(pre) ; write('Do you want to call, raise or fold?')).

test :-
  play,
  go,
  check,
  check,
  check,
  check.

check :-
  deck(Deck),
  pokertable([_, _, _, Last_to_Act]),
  (   length(Deck, 48), Last_to_Act == player -> nl, dealflop(Deck), write('Flop is: '), flop, ai_magic(check)
    ; length(Deck, 44), Last_to_Act == player ->  nl, dealturn(Deck), write('Turn is: '), turn, ai_magic(check)
    ; length(Deck, 42), Last_to_Act == player ->  nl, dealriver(Deck), write('River is: '), river, ai_magic(check)
    ; length(Deck, 40), Last_to_Act == player -> nl, player1Cards(X), player2Cards(Y), whoWon(X,Y)
    ; Last_to_Act == ai -> nl, ai_magic(check)
    ),
    pt.

bet :-
  write('You bet!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], _]),
  (B1 > B2 -> X = B1
  ; X = B2),
  Y is Stack - X,
  Z is Pot + X,
  setPokertable([Y, Z, [B1, B2], ai]),
  ai_magic(bet),
  pt.

raise :-
  deck(Deck),
  write('You raise!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], _]),
  (   B1 > B2 -> X = B1
    ; X = B2),
  Y is Stack - X*2,
  Z is Pot + X*2,
  S is Stack - X*1.5,
  P is Pot + X*1.5,
  (length(Deck, 48) -> setPokertable([S, P, [B1, B2], ai])
  ; setPokertable([Y, Z, [B1, B2], ai])),
  ai_magic(raise),
  pt.

call :-
  deck(Deck),
  write('You call!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], Last_to_Act]),
  (   B1 > B2 -> X = B1, W = player
    ; X = B2, W = ai),
  Y is Stack - X,
  Z is Pot + X,
  S is Stack - X/2,
  P is Pot + X/2,
  (   length(Deck, 48), Last_to_Act == player -> setPokertable([Y, Z, [B1, B2], W]), dealflop(Deck), write('Flop is: '), flop
    ; length(Deck, 48), Last_to_Act == ai -> A = 5, setPokertable([S, P, [B1, B2], Last_to_Act]), ai_magic(check)
    ; length(Deck, 44) -> setPokertable([Y, Z, [B1, B2], W]), dealturn(Deck), write('Turn is: '), turn
    ; length(Deck, 42) -> setPokertable([Y, Z, [B1, B2], W]), dealriver(Deck), write('River is: '), river
    ; length(Deck, 40) -> setPokertable([Y, Z, [B1, B2], W]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2)
    ),
  (  length(Deck, 40) -> !
    ;  W == player, A \== 5 -> ai_magic(check)
    ; A \== 5 -> nl, write('Do you want to check, bet or fold?')
    ; !),
  pt.

fold :-
  write('You lost'),
  pt.
