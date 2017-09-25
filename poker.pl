
:-use_module(library(random)).

% There are 4 different colors
color(clubs).
color(diamonds).
color(hearts).
color(spades).

% There are 13 different values
value(ace).
value(2).
value(3).
value(4).
value(5).
value(6).
value(7).
value(8).
value(9).
value(10).
value(jack).
value(queen).
value(king).

% A card is defined by 2 terms, Color and Value
card(Color, Value):-
  color(Color),
  value(Value).

% Gör detta snyggare Daniel
deck(L):-
  createDeck(L1),
  shuffle(L1, L2),
  shuffle(L2, L3),
  shuffle(L3, L4),
  shuffle(L4, L5),
  shuffle(L5, L6),
  shuffle(L6, L7),
  cup(L7,L).


% A list containing all the cards
createDeck(L):-
  findall(card(X, Y), card(X, Y), L).

% Shuffle cards and finish by kupera
shuffle(L1, Result):-
  cut(L1, L2, L3),
  merge(L2, L3, Result, 1).

cup(L, Res):-
    cut(L, L1, L2),
    append(L1, L2, Res). % Använd inte Append

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
