:-module(pokerrules,[check/3, checkHand/3, handValue/1,
                    sortByNumber/2, sortByColor/2,
                    straight_flush/4 , four_of_a_kind/3,
                    full_house/3, flush/3, straight/3,
                    three_of_a_kind/3, two_pair/3, whoWon/2,
                    pair/3, highest_card/3, reverse/2, remove/3, nothing/3]).
:-use_module(dealer).

/*Defining the value of the hand, the
lower the number the better the hand*/
handValue('a straight flush ', 1).
handValue('four of a kind ', 2).
handValue('a full house ', 3).
handValue('a flush ', 4).
handValue('a straight ', 5).
handValue('three of a kind ', 6).
handValue('two pair ', 7).
handValue('a pair ', 8).
handValue('highest card ', 9).

%Evaluates your hand
check(L, FiveBest, V):-
  sortByNumber(L, Res),
  checkHand(Res, FiveBest, V),!.

whoWon(Hand1, Hand2):-
check(Hand1, FiveBest1, V1),
check(Hand2, FiveBest2, V2),
winner(V1,V2,Res, FiveBest1,FiveBest2),
handValue(HV1, V1),
handValue(HV2, V2),
(Res == won -> write('You win!'),nl, write('You got ') , write(HV1), write('with the hand '), write(FiveBest1), nl, write(' The AI got '), write(HV2), write('with the hand '), write(FiveBest2)
; Res == lost -> write('You looose!'),nl, write('You got ') , write(HV1), write('with the hand '), write(FiveBest1), nl, write('The AI got '), write(HV2), write('with the hand '), write(FiveBest2)
; Res == tie -> write('Ooooh, both had same hand! '),nl, write('Both got'), write(HV1), write('with the hand '), write(FiveBest1), nl, write('Its a tie!')).

%Checks for best first
checkHand(L, FiveBest, V):-
sortByColor(L, Res), straight_flush(Res, FiveBest, [] ,V), !;
four_of_a_kind(L, V1, V), remove(L, V1, [V5|_]), FiveBest = [V1,V1,V1,V1,V5], !;
full_house(L, FiveBest, V), !;
sortByColor(L, Res),flush(Res, FiveBest, V), !;
straight(L, FiveBest, V, _), !;
three_of_a_kind(L, V1, V), remove(L, V1, [V4,V5|_]), FiveBest = [V1,V1,V1,V4,V5], !;
two_pair(L, [V1, V2], V), remove(L, V1, V2, [V5|Res]), FiveBest = [V1,V1,V2,V2,V5], !;
pair(L, V1, V), remove(L, V1, [V3,V4,V5|_]), FiveBest = [V1, V1, V3, V4, V5], !;
nothing(L, FiveBest, V), !.

%Decide a winner!
winner(V, V, tie, [],[]):- !.
winner(V1, V2, won, _, _):-
  V1 < V2,!.
winner(V1, V2, lost, _, _):-
  V1 > V2,!.
winner(V, V, X, FiveBest1, FiveBest2):-
  tie(FiveBest1,FiveBest2, X), !.

tie([], [], tie):- !.
tie([V1|_], [V2|_], won):-
  V1 > V2,!.
tie([V1|_], [V2|_], lost):-
  V1 < V2,!.
tie([V|R1], [V|R2], X):-
  tie(R1, R2, X).



%Sorts the hand by number
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

%Merging two lists
oneList([], [], []).
oneList([], [H|T],[H|Res]):-
  oneList([], T, Res).
oneList([H|T], L2, [H|Res]):-
  oneList(T, L2, Res).

/* straight_flush(+, -, -)
   uses flush and straight */
straight_flush([card(X, 5), card(X, 4), card(X, 3), card(X, 2)|_], [5, 4, 3, 2, 14], ListOfAces, 1) :-
  write(ListOfAces),
  member(card(X, 14), ListOfAces).
straight_flush([card(X, V1), card(X, V2), card(X, V3), card(X, V4), card(X, V5)|_], [V1, V2, V3, V4, V5], _, 1) :-
  V1 is V2 + 1,
  V2 is V3 + 1,
  V3 is V4 + 1,
  V4 is V5 + 1.
straight_flush([card(_,X)|R], FiveBest, ListOfAces, 1) :-
  X=\=14,
  straight_flush(R, FiveBest, ListOfAces, 1).
straight_flush([card(X,14)|R], FiveBest, ListOfAces, 1) :-
  straight_flush(R, FiveBest, [card(X,14)|ListOfAces], 1).

%four_of_a_kind(+, -)
four_of_a_kind([card(_,V1), card(_,V1), card(_,V1), card(_,V1)|_], V1, 2).
four_of_a_kind([card(_,_)|R], V1, 2) :-
  four_of_a_kind(R, V1, 2).

%full_house(+, -)
full_house([card(_,V1), card(_,V1), card(_,V1), card(_,V2), card(_,V2)|_], [V1,V1,V1,V2,V2], 3).
full_house([card(_,V2), card(_,V2), card(_,V1), card(_,V1), card(_,V1)|_], [V1,V1,V1,V2,V2], 3).
full_house([card(_,_)|R], V1, 3) :-
  full_house(R, V1, 3).

%flush(+, -, -)
flush([card(X, V1), card(X, V2), card(X, V3), card(X, V4), card(X, V5)|_], [V1,V2,V3,V4,V5], 4).
flush([card(_,_)|R], _, 4) :-
  flush(R, _, 4).

%straight(+, -)
straight([C1,Rest], FiveBest, 5):-    %If we got an ace, it needs to be the first card in the sorted list
  C1 = card(_,14), straight([C1|Rest], FiveBest, 5, yes);
  straigth([C1|Rest], FiveBest, 5, no).
straight([card(_, 5), card(_, 4), card(_, 3), card(_, 2)|_], [5, 4, 3, 2, 14], 5, yes).
straight([card(_, V1), card(_, V2), card(_, V3), card(_, V4), card(_, V5)|_], [V1, V2, V3, V4, V5], 5, _) :-
  V1 is V2 + 1,
  V2 is V3 + 1,
  V3 is V4 + 1,
  V4 is V5 + 1.
straight([card(_,_)|R], V1, 5, GotAce) :-
  straight(R, V1, 5, GotAce).

%three_of_a_kind(+, -)
three_of_a_kind([card(_,V1), card(_,V1), card(_,V1)|_], V1, 6).
three_of_a_kind([card(_,_)|R], V1, 6) :-
  three_of_a_kind(R, V1, 6).

%two_pair(+, -, -), uses pair
two_pair(Hand, [V1,V2], 7) :-
findall(X,pair(Hand,X,_),[V1,V2|_]).

%pair(+, -)
pair([card(_,V1), card(_,V1)|_], V1, 8).
pair([card(_,_)|R], V1, 8) :-
  pair(R, V1, 8).

nothing([card(_,V1),card(_,V2),card(_,V3),card(_,V4),card(_,V5)|_], FiveBest, 9) :-
  FiveBest = [V1,V2,V3,V4,V5], !.

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

%Remove an element from a lists

remove([], _, []):- !.
remove([card(_, Value)|T], Value, Res):-
  remove(T, Value, Res), !.
remove([card(_, Wrong)|T], Value, [Wrong|Res]):-
  remove(T, Value, Res), !.

remove([],_, _, []):- !.
remove([card(_, Value1)|T], Value1, Value2, Res):-
  remove(T, Value1,Value2, Res),!.
remove([card(_, Value2)|T], Value1, Value2, Res):-
    remove(T, Value1,Value2, Res),!.
remove([card(_, Wrong)|T], Value1, Value2, [Wrong|Res]):-
  remove(T, Value1, Value2, Res),!.
