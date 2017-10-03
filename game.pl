
:-module(game,[echo/0, done/0]).
:-use_module(dealer).
:-use_module(pokerrules).

p1 :- listing(player1).
p2 :- listing(player2).
flop :- listing(flop).
turn :- listing(turn).
river :- listing(river).
playerstack :- listing(playerstack).

echo :-
  write('>> '),
  read(X),
  call(X),
  echo.

play :-
  setPlayerstack(1000),nl,
  write('Hello and welcome to this uber good poker game'), nl,
  write('You start with a stack of 1000'), nl,
  write('The different commands is "check", "bet", "call" or "fold"'), nl,
  write('  .-------------. '),nl,
  write(' |               | '),nl,
  write('|                 |'),nl,
  write(' |               | '),nl,
  write('  "-------------"  '),nl,
  write('This is table').


go :-
  createDeck(X),
  dealtp(X),
  %player1Cards(P1),
  %sortByNumber(P1, P1s),
  p1,
  write('check, raise or fold?'),
  nl.

%First check before turn
check :-
    nl,
    deck(Deck),
    length(Deck, 48),
    dealflop(Deck),
    flop,
    write('check, raise or fold?'),
    nl.
%Second check before river
check :-
    nl,
    deck(Deck),
    length(Deck, 44),
    dealturn(Deck),
    turn,
    write('check, raise or fold?'),
    nl.
%Last check
check :-
    nl,
    deck(Deck),
    length(Deck, 42),
    dealriver(Deck),
    river,
    write('check, raise or fold?'),
    nl.

bet :-
    nl,
    deck(Deck),
    length(Deck, 42),
    dealriver(Deck),
    river,
    write('check, raise or fold?'),
    nl.

fold :-
write('You lost').


player1Cards(D) :-
  player1([X, Y]),
  flop([Z, W, T]),
  turn([S]),
  river([Q]),
  D = [X, Y, Z, W, T, S, Q].
