
:-module(dealer,[dealtp/4,dealflop/3,dealturn/3, dealriver/2,deck/1]).
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
  random_permutation(Unshuffled,Shuffled).

% A list containing all the cards
createDeck(L):-
  findall(card(X, Y), card(X, Y), L).
