
:-module(game,[echo/0, done/0, go/0]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(ai).

p1 :- player1(X), write(X).
p2 :- player1(X), write(X).
flop :- flop(X), write(X).
turn :- turn(X), write(X).
river :- river(X), write(X).

echo :-
  write('>> '),
  read(X),
  call(X),
  echo.

play :-
  setPokertable([1000, 0, [10,20], ai]),nl,
  write('Hello and welcome to this uber good poker game'), nl,
  write('You start with a stack of 1000'), nl,
  write('The different commands is "check", "bet", "call" or "fold"'), nl,
  write('  .-------------. '),nl,
  write(' |               | '),nl,
  write('|                 |'),nl,
  write(' |               | '),nl,
  write('  "-------------"  '),nl,
  write('This is table'),nl,nl,nl.


go :-
  createDeck(X),
  dealtp(X),
  pokertable([Stack, Pot, [B1,B2], Last_to_act]),
  Y is Stack - B2,
  Z is B1+B2,
  (B1 > B2 -> W = ai
  ; W = player),
  setPokertable([Y, Z, [B2,B1], W]),
  %ai_magic(pre),
  write('Your hand is: '),
  p1,
  nl,
  ai_magic(pre).

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
  (   length(Deck, 48), Last_to_Act == player -> write('You check!'), nl, nl, dealflop(Deck), write('Flop is: '), flop, ai_magic(check), write('Do you want to check, bet or fold?')
    ; length(Deck, 48), Last_to_Act == ai -> write('You check!'), nl, ai_magic(check)
    ; length(Deck, 44), Last_to_Act == player -> write('You check!'),nl, nl, dealturn(Deck), write('Turn is: '), turn, ai_magic(check), write('Do you want to check, bet or fold?')
    ; length(Deck, 44), Last_to_Act == ai -> write('You check!'), nl, ai_magic(check)
    ; length(Deck, 42), Last_to_Act == player -> write('You check!'),nl, nl, dealriver(Deck), write('River is: '), river, ai_magic(check), write('Do you want to check, bet or fold?')
    ; length(Deck, 42), Last_to_Act == ai -> write('You check!'), nl, ai_magic(check)
    ; length(Deck, 40), Last_to_Act == player -> write('You check!'), nl, nl, player1Cards(X),player2Cards(Y), whoWon(X,Y)
    ; length(Deck, 40), Last_to_Act == ai -> write('You check!'), nl, ai_magic(check)
    ), nl.

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
  nl.

raise :-
  write('You raise!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], _]),
  (B1 > B2 -> X = B1
  ; X = B2),
  Y is Stack - X*2,
  Z is Pot + X*2,
  setPokertable([Y, Z, [B1, B2], ai]),
  %INSERT AI MAGIC HERE
  nl.

call :-
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], Last_to_Act]),
  (B1 > B2 -> X = B1, W = player
  ; X = B2, W = ai),
  Y is Stack - X,
  Z is Pot + X,
  S is Stack - 10,
  P is Pot + 10,
  %setPokertable([Y, Z, [B1, B2], W]),
  (   length(Deck, 48), Last_to_Act == player -> setPokertable([Y, Z, [B1, B2], W]), dealflop(Deck), write('Flop is: '), flop, write('check, raise or fold?')
    ; length(Deck, 48), Last_to_Act == ai -> setPokertable([S, P, [B1, B2], Last_to_Act]), ai_magic(call)
    ; length(Deck, 44) -> setPokertable([Y, Z, [B1, B2], W]), dealturn(Deck), write('Turn is: '), turn, write('check, raise or fold?')
    ; length(Deck, 42) -> setPokertable([Y, Z, [B1, B2], W]), dealriver(Deck), write('River is: '), river, write('check, raise or fold?')
    ; length(Deck, 40) -> setPokertable([Y, Z, [B1, B2], W]), write('Someone won')
    ),
    %INSERT AI MAGIC HERE
    nl.

fold :-
write('You lost'),
nl.


player1Cards(D) :-
  player1([X, Y]),
  flop([Z, W, T]),
  turn([S]),
  river([Q]),
  D = [X, Y, Z, W, T, S, Q].

  player2Cards(D) :-
    player2([X, Y]),
    flop([Z, W, T]),
    turn([S]),
    river([Q]),
    D = [X, Y, Z, W, T, S, Q].
