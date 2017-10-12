
:-module(dealer,[dealtp/1, dealflop/1,
                  dealturn/1, dealriver/1,
                  createDeck/1, setListDeck/1,
                  setP1card/1, setP2card/1, setFlop/1,
                  setTurn/1, setRiver/1, setPokertable/1,
                  deck/1, player1/1, player2/1, flop/1,
                  turn/1, river/1, pokertable/1,
                  player1Cards/1, player2Cards/1]).

:-use_module(library(random)).
:-use_module(library(system)).

%we use dynamic predicates
:- dynamic deck/1.
:- dynamic player1/1.
:- dynamic player2/1.
:- dynamic flop/1.
:- dynamic turn/1.
:- dynamic river/1.
:- dynamic pokertable/1.

% There are 4 different colors, clubs, diamonds, hearts and spades
color('♣').
color('♦').
color('♥').
color('♠').

% There are 13 different values
value(14).
value(2).
value(3).
value(4).
value(5).
value(6).
value(7).
value(8).
value(9).
value(10).
value(11).
value(12).
value(13).

% dealtp(Deck+), sets P1cards and P2cards aswell as deck
dealtp([C1,C2,C3,C4|Deck]) :-
  setP1card([C1, C3]),
  setP2card([C2, C4]),
  setListDeck(Deck).

% dealflop(Deck+), sets flop and rest of deck
dealflop([_,C2,C3,C4|Deck]) :-
  setFlop([C2, C3, C4]),
  setListDeck(Deck).

% dealturn(Deck+), sets turn and rest of deck
dealturn([_,C2|Deck]) :-
  setTurn([C2]),
  setListDeck(Deck).

% dealriver(Deck+), sets river and rest of the deck
dealriver([_,C2|Deck]) :-
  setRiver([C2]),
  setListDeck(Deck).

%player1Cards(Cards-), gets the asserted predicates and returns them as one list
player1Cards(Cards) :-
  player1([X, Y]),
  flop([Z, W, T]),
  turn([S]),
  river([Q]),
  Cards = [X, Y, Z, W, T, S, Q].

%player2Cards(Cards-), gets the asserted predicates and returns them as one list
player2Cards(Cards) :-
  player2([X, Y]),
  flop([Z, W, T]),
  turn([S]),
  river([Q]),
  Cards = [X, Y, Z, W, T, S, Q].

/*card(Color-,Value-)
A card is defined by 2 terms, Color and Value*/
card(Color, Value):-
  color(Color),
  value(Value).

/*createDeck(Shuffled-)
Create a deck then shuffles it.*/
createDeck(Shuffled):-
  findall(card(X, Y), card(X, Y), L), %finds all possible cards and puts in the list L
  now(Randomint), %gets current date and time
  setrand(Randomint), %sets the seed to current
  random_permutation(L, Shuffled).  %shuffles the deck L 

%functions to retract then assert new Deck, P1cards, P2cards, flop, turn river and pokertable
setListDeck(_) :-
  retract(deck(_)),
  fail.
setListDeck(X) :-
  assert(deck(X)).

setP1card(_) :-
  retract(player1(_)),
  fail.
setP1card(X) :-
  assert(player1(X)).

setP2card(_) :-
  retract(player2(_)),
  fail.
setP2card(X) :-
  assert(player2(X)).

setFlop(_) :-
  retract(flop(_)),
  fail.
setFlop(X) :-
  assert(flop(X)).

setTurn(_) :-
  retract(turn(_)),
  fail.
setTurn(X) :-
  assert(turn(X)).

setRiver(_) :-
  retract(river(_)),
  fail.
setRiver(X) :-
  assert(river(X)).

setPokertable(_) :-
  retract(pokertable(_)),
  fail.
setPokertable(X) :-
  assert(pokertable(X)).
