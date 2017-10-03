
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
    deck(Deck),
    (   length(Deck, 48) -> dealflop(Deck), flop    %If decksize is 48, deal flop
      ; length(Deck, 44) -> dealturn(Deck), turn    %If decksize is 44, deal turn
      ; length(Deck, 42) -> dealriver(Deck), river  %If decksize is 42, deal river
      ; length(Deck, 40) -> write("Someone won")    %
      ),
    write('check, raise or fold?'),
    nl.

bet :-
    deck(Deck),
    (   length(Deck, 48) -> dealflop(Deck), flop    %If decksize is 48, deal flop
      ; length(Deck, 44) -> dealturn(Deck), turn    %If decksize is 44, deal flop
      ; length(Deck, 42) -> dealriver(Deck), river  %If decksize is 42, deal river
      ; length(Deck, 40) -> write("Someone won")    %
      ),
    write('check, raise or fold?'),
    nl.

raise :-
    deck(Deck),
  (   length(Deck, 48) -> dealflop(Deck), flop    %If decksize is 48, deal flop
    ; length(Deck, 44) -> dealturn(Deck), turn    %If decksize is 44, deal flop
    ; length(Deck, 42) -> dealriver(Deck), river  %If decksize is 42, deal river
    ; length(Deck, 40) -> write("Someone won")    %
    ),
  write('check, raise or fold?'),
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
