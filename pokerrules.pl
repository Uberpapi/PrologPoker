:-module(pokerrules,[check/1, checkHand/3, handValue/1,
                    sortByNumber/2, sortByColor/2,
                    straight_flush/4, four_of_a_kind/3,
                    full_house/3, flush/4, straight/3,
                    three_of_a_kind/3, two_pair/3,
                    pair/3, highest_card/3, reverse/2]).
:-use_module(dealer).

/*Defining the value of the hand, the
lower the number the better the hand*/
handValue(straight_flush, 1).
handValue(four_of_a_kind, 2).
handValue(full_house, 3).
handValue(flush, 4).
handValue(straight, 5).
handValue(three_of_a_kind, 6).
handValue(two_pair, 7).
handValue(pair, 8).
handValue(nothing, 9).

%Evaluates your hand
check(L):-
  sortByNumber(L, Res),
  checkHand(Res, Value, V),
  handValue(X, V),
  print(X).

checkHand(L, Value, V):-
sortByColor(L, Res), straight_flush(Res, _, Value, V);
four_of_a_kind(L, Value, V);
full_house(L, Value, V);
sortByColor(L, Res), flush(Res, _, Value, V);
straight(L, Value, V);
three_of_a_kind(L, Value, V);
two_pair(L, Value, V);
pair(L, Value, V).

%Sorts the hand by
sortByNumber(L, Sorted):-
  sortByNumber(L, [], Sorted).
sortByNumber([], Sorted, Sorted).
sortByNumber([Card|T], L2, Sorted):-
  insertNumber(Card, L2, L3),
  sortByNumber(T, L3, Sorted).

insertNumber(card(C1,V1), [card(C2,V2)|T], [card(C2,V2)|F]):-
  V1<V2,
  insertNumber(card(C1,V1), T, F).
insertNumber(card(C1,V1), [card(C2,V2)|T], [card(C1,V1),card(C2,V2)|T]):-
  V1>=V2.
insertNumber(card(C,V), [], [card(C,V)]).

%Sorts the hand by color
sortByColor(L, Sorted):-
  reverse(L, Res),
  sortByColor(Res, [], Sorted).
sortByColor([], Sorted, Sorted).
sortByColor([Card|T], L2, Sorted):-
  insertColor(Card, L2, L3),
  sortByColor(T, L3, Sorted).

insertColor(card(C1,V1), [card(C2,V2)|T], [card(C2,V2)|F]):-
  C1 \== C2,
  insertColor(card(C1,V1), T, F).
insertColor(card(C1,V1), [card(C2,V2)|T], [card(C1,V1),card(C2,V2)|T]):-
  C1==C2.
insertColor(card(C,V), [], [card(C,V)]).

%Merging four lists into one
oneList([], [], []).
oneList([], [H|T],[H|Res]):-
  oneList([], T, Res).
oneList([H|T], L2, [H|Res]):-
  oneList(T, L2, Res).

%straight_flush(+, -, -)
%uses flush and straight
straight_flush(Hand, Color, Value, 1) :-
  flush(Hand, Color, Value, _),
  straight(Hand, Value, _).

%four_of_a_kind(+, -)
four_of_a_kind([card(_,V1), card(_,V1), card(_,V1), card(_,V1)|_], V1, 2).
four_of_a_kind([card(_,_)|R], V1, _) :-
  four_of_a_kind(R, V1, _).

%full_house(+, -)
full_house([card(_,V1), card(_,V1), card(_,V1), card(_,V2), card(_,V2)|_], V1, 3).
full_house([card(_,V2), card(_,V2), card(_,V1), card(_,V1), card(_,V1)|_], V1, 3).
full_house([card(_,_)|R], V1, _) :-
  full_house(R, V1, _).

%flush(+, -, -)
flush([card(X, Y), card(X, _), card(X, _), card(X, _), card(X, _)|_], X, Y, 4).
flush([card(_,_)|R], X, Y, _) :-
  flush(R, X, Y, _).

%straight(+, -)
straight([card(_, V1), card(_, V2), card(_, V3), card(_, V4), card(_, V5)|_], V1, 5) :-
  V1 is V2 + 1,
  V2 is V3 + 1,
  V3 is V4 + 1,
  V4 is V5 + 1.
straight([card(_,_)|R], V1, _) :-
  straight(R, V1, _).

%three_of_a_kind(+, -)
three_of_a_kind([card(_,V1), card(_,V1), card(_,V1)|_], V1, 6).
three_of_a_kind([card(_,_)|R], V1, _) :-
  three_of_a_kind(R, V1, _).

%two_pair(+, -, -)
%uses pair
two_pair(Hand, [V1, V2], 7) :-
  findall(X, pair(Hand, X, _), [V1, V2|_]).

%pair(+, -)
pair([card(_,V1), card(_,V1)|_], V1, 8).
pair([card(_,_)|R], V1) :-
  pair(R, V1).

%highest_card(+, +, -)
highest_card([card(_,V1)|_], [card(_,V2)|_], V1) :-
  V1 > V2.
highest_card([card(_,V1)|_], [card(_,V2)|_], V1) :-
  V2 > V1.
highest_card([card(_,V1)|H1], [card(_,V2)|H2], V3) :-
  V1 == V2,
  highest_card(H1, H2, V3).

%Reversing a list
reverse(L, Res):-
  reverse(L, [], Res).
reverse([], Res, Res).
reverse([H|T], L2, Res):-
  reverse(T, [H|L2], Res).
