﻿
:-use_module(library(random)).

% There are 4 different colors
color(clubs).
color(diamonds).
color(hearts).
color(spades).

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

game(P1,P2,Flop,Turn,River):-
  deck(Shuffled),
  dealtp(Shuffled, P1 , P2, Restfromtp),
  dealflop(Restfromtp, Flop, Restfromflop),
  dealturn(Restfromflop, Turn, Restfromturn),
  dealriver(Restfromturn, River).

dealtp([C1,C2,C3,C4|Deck], [C1,C3], [C2,C4], Deck).

dealflop([_,C2,C3,C4|Deck],[C2,C3,C4], Deck).

dealturn([_,C2|Deck], [C2], Deck).

dealriver([_,C2|_],[C2]).


% A card is defined by 2 terms, Color and Value
card(Color, Value):-
  color(Color),
  value(Value).

% Create a deck then shuffle it.
deck(Shuffled):-
  createDeck(Unshuffled),
  shuffle(Unshuffled, Shuffled, 6).



% A list containing all the cards
createDeck(L):-
  findall(card(X, Y), card(X, Y), L).

% Shuffle cards X times and then end with a cup
shuffle(L, Result, 0):-
  cup(L, Result).
shuffle(L1, Res, X):-
  X\==0,
  Y is X-1,
  cut(L1, L2, L3),
  merge(L2, L3, L4, 1),
  shuffle(L4, Res, Y).


%Cuping the deck and then stack one half on the other
cup(L, Res):-
  Res == [],
  cut(L, L1, L2),
  cup(L1, L2).
cup([H1|T1], [H1|T2]):-
  (H1, T2 \== []),
  cup(T1, T2).
cup(L, _):-
  L==[].


/* cutting the deck by a random number and putting
them together and returning a complete deck */
cut(L1, L2, L3):-
  random(20, 32, RanNum),
  cut(L1, L2, L3, RanNum).
cut(T, [], L3, 0):- L3 = T.
cut([H|T], [H|L2], L3, RanNum):-
 Num is RanNum - 1, cut(T, L2, L3, Num).


% Merging of the cut list with a random element
merge([], [], [], _).
merge([], [H|T], [H|Result], _):-
  merge([], T, Result, _).
merge([H|T], [], [H|Result], _):-
  merge(T, [], Result, _).
merge([H|T], L, [H|Result], 1):-
  L\==[],
  random(1, 3, RanCut),
  merge(L, T, Result, RanCut).
merge([H|T], L, [H|Result], 2):-
  L\==[],
  random(1, 3, RanCut),
  merge(T, L, Result, RanCut).
