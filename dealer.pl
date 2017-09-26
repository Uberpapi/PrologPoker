﻿
:-module(dealer,[dealtp/2, dealflop/2,
                  dealturn/2, dealriver/1,
                  createDeck/1,
                  setListDeck/1, setP1card/1,
                  setP2card/1, setFlop/1,
                  setTurn/1, setRiver/1, deck/1,
                  player1/1, player2/1, flop/1,
                  turn/1, river/1]).

:-use_module(library(random)).

:- dynamic deck/1.
:- dynamic player1/1.
:- dynamic player2/1.
:- dynamic flop/1.
:- dynamic turn/1.
:- dynamic river/1.


% There are 4 different colors
color('♣').
color('♦').
color('♥').
color('♠').

% There are 13 different values
value('A').
value(2).
value(3).
value(4).
value(5).
value(6).
value(7).
value(8).
value(9).
value(10).
value('J').
value('Q').
value('K').

% dealtp(+, -, -, -).
dealtp([C1,C2,C3,C4|Deck], Deck) :-
  setP1card([C1, C3]),
  setP2card([C2, C4]),
  setListDeck(Deck).

% dealflop(+, -, -).
dealflop([_,C2,C3,C4|Deck], Deck) :-
  setFlop([C2, C3, C4]),
  setListDeck(Deck).

% dealturn(+, -, -).
dealturn([_,C2|Deck], Deck) :-
  setTurn([C2]),
  setListDeck(Deck).

% dealriver(+, -).
dealriver([_,C2|_]) :-
  setRiver([C2]).

% A card is defined by 2 terms, Color and Value
% card(-, -).
card(Color, Value):-
  color(Color),
  value(Value).

% Create a deck then shuffle it.
% deck(-).
createDeck(Shuffled):-
  findall(card(X, Y), card(X, Y), L),
  random_permutation(L, Shuffled).

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
