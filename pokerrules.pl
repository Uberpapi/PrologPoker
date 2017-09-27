:-module(pokerrules,[sorts/2, straight_flush/3, flush/3, straight/2]).
:-use_module(dealer).


successor(straight_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).
successor(full_house, flush).
successor(flush, straight).
successor(straight, three_of_a_kind).
successor(three_of_a_kind, two_pair).
successor(two_pair, pair).
successor(pair, highest_card).


sorts(L, Sorted):-
  sorts(L, [], Sorted).
sorts([], L, Sorted):-
  sortByColor(L, Sorted).
sorts([Card|T], L2, Sorted):-
  insertNumber(Card, L2, L3),
  sorts(T, L3, Sorted).

insertNumber(card(C1,V1), [card(C2,V2)|T], [card(C2,V2)|F]):-
  V1>V2,
  insertNumber(card(C1,V1), T, F).
insertNumber(card(C1,V1), [card(C2,V2)|T], [card(C1,V1),card(C2,V2)|T]):-
  V1=<V2.
insertNumber(card(C,V), [], [card(C,V)]).

sortByColor(L, Sorted):-
  sortByColor(L, [], Sorted).
sortByColor([], Sorted, Sorted).
sortByColor([Card|T], L2, Sorted):-
  insertColor(Card, L2, L3),
  sortByColor(T, L3, Sorted).

insertColor(card(C1,V1), [card(C2,V2)|T], [card(C2,V2)|F]):-
  C1\==C2,
  insertColor(card(C1,V1), T, F).
insertColor(card(C1,V1), [card(C2,V2)|T], [card(C1,V1),card(C2,V2)|T]):-
  C1==C2.
insertColor(card(C,V), [], [card(C,V)]).


oneList([], [], []).
oneList([], [H|T],[H|Res]):-
  oneList([], T, Res).
oneList([H|T], L2, [H|Res]):-
  oneList(T, L2, Res).

%straight_flush(+, -, -) uses flush and straight
straight_flush(Hand, Color, Value) :-
  flush(Hand, Color, Value),
  straight(Hand, Value).

%flush(+, -, -)
flush([card(X, Y), card(X, _), card(X, _), card(X, _), card(X, _)|_], X, Y).

flush([card(_,_)|R], X, Y) :-
  flush(R, X, Y).

%straight(+, -)
straight([card(_, V1), card(_, V2), card(_, V3), card(_, V4), card(_, V5)|_], V1) :-
  V1 is V2 + 1,
  V2 is V3 + 1,
  V3 is V4 + 1,
  V4 is V5 + 1.

straight([card(_,_)|R], V1) :-
  straight(R, V1).
