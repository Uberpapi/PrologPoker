:-module(pokerrules,[check/3, checkHand/3, handValue/1,
                    sortByNumber/2, sortByColor/2,
                    whoWon/2, doubleRemove/2]).
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

/*whoWon(Hand1+, Hand2+),
Checks who's hand is better and decides a winner.
Newstack is used to check if player has beat the ai or lost vs the ai*/
whoWon(Hand1, Hand2):-
pokertable([Stack, Pot, B1, B2, Handsplayed]),
check(Hand1, FiveBest1, V1),
check(Hand2, FiveBest2, V2),
winner(V1,V2,Res, FiveBest1,FiveBest2),
handValue(HV1, V1),
handValue(HV2, V2),
retractall(deck(_)),  % We award the winner the pot, if there is a tie both players regain their bets
(Res == won -> Newstack is Stack + Pot, setPokertable([Newstack, 0, B1, B2, Handsplayed]), format('You win ~d$~nYou got ~w~nThe AI got ~w~n', [Pot,HV1,HV2])
; Res == lost -> Newstack is Stack, format('You looose!~nYou got ~w~nThe AI got ~w~n',[HV1,HV2])
; Res == tie -> Newstack is Stack + Pot/2, setPokertable([Newstack, 0, B1, B2, Handsplayed]),format('Ooooh, bot had same hand! You split ~d$~nBoth got ~w~nIts a tie!~n', [Pot, HV1])),
(Newstack > 1999 -> format('~nCongratulation, you beat the AI in ~w hands!~nIf you want to play again write "play."', [Handsplayed])
; Newstack < 1 -> format('~nWhat the hell, you lost after ~w hands!~nThe AI is not THAT good.~n~nIf you want to play again write "play."', [Handsplayed])
;write('Write "go" to deal the next hand'), nl).

%checkHand(Hand+, Bestcards-, value-), Checks for hands, best first
checkHand(L, FiveBest, V):-
sortByColor(L, Res), straight_flush(Res, FiveBest, V), !;
four_of_a_kind(L, V1, V), remove(L, V1, [V5|_]), FiveBest = [V1,V1,V1,V1,V5], !;
full_house(L, FiveBest, V), !;
sortByColor(L, Res),flush(Res, FiveBest, V), !;
doubleRemove(L, Res), straight(Res, FiveBest, V), !;
three_of_a_kind(L, V1, V), remove(L, V1, [V4,V5|_]), FiveBest = [V1,V1,V1,V4,V5], !;
two_pair(L, [V1, V2], V), remove(L, V1, V2, [V5|Res]), FiveBest = [V1,V1,V2,V2,V5], !;
pair(L, V1, V), remove(L, V1, [V3,V4,V5|_]), FiveBest = [V1, V1, V3, V4, V5], !;
nothing(L, FiveBest, V), !.

/*winner(HandvalueP1+, HandvalueP2+, Decision-, BestcardsP1+, BestcardsP2+)
Decide a winner*/
winner(V, V, tie, [],[]):- !.
winner(V1, V2, won, _, _):-
  V1 < V2,!.
winner(V1, V2, lost, _, _):-
  V1 > V2,!.
winner(V, V, X, FiveBest1, FiveBest2):-
  tie(FiveBest1,FiveBest2, X), !.

/*tie(Hand1+, Hand2+, decision-)
If there is a tie, we decide who wins here*/
tie([], [], tie):- !. %if no player has one card higher than the other its a tie
tie([V1|_], [V2|_], won):-
  V1 > V2,!.
tie([V1|_], [V2|_], lost):-
  V1 < V2,!.
tie([V|R1], [V|R2], X):-
  tie(R1, R2, X).

%sortByNumber(Hand+, Sorted-), Sorts the hand by number
sortByNumber(L, Sorted):-
  sortByNumber(L, [], Sorted).
sortByNumber([], Sorted, Sorted).
sortByNumber([Card|T], L2, Sorted):-
  insertNumber(Card, L2, L3),
  sortByNumber(T, L3, Sorted).

%inserts the cards depending on values
insertNumber(card(C1,V1), [card(C2,V2)|T], [card(C2,V2)|F]):-
  V1<V2,
  insertNumber(card(C1,V1), T, F).
insertNumber(card(C1,V1), [card(C2,V2)|T], [card(C1,V1),card(C2,V2)|T]):-
  V1>=V2.
insertNumber(card(C,V), [], [card(C,V)]).

%sortByColor(Hand+, Sorted-), Sorts the hand by color
sortByColor(L, Sorted):-
  reverse(L, Res),
  sortByColor(Res, [], Sorted).
sortByColor([], Sorted, Sorted).
sortByColor([Card|T], L2, Sorted):-
  insertColor(Card, L2, L3),
  sortByColor(T, L3, Sorted).

%inserts card depending on color
insertColor(card(C1,V1), [card(C2,V2)|T], [card(C2,V2)|F]):-
  C1 \== C2,
  insertColor(card(C1,V1), T, F).
insertColor(card(C1,V1), [card(C2,V2)|T], [card(C1,V1),card(C2,V2)|T]):-
  C1==C2.
insertColor(card(C,V), [], [card(C,V)]).

/* straight_flush(Hand+, Cardvalues-, value-)
   uses flush and straight */
straight_flush(Sorted, [V1, V2, V3, V4, V5], 1) :-
  flush(Sorted, [V1,V2,V3,V4,V5], _),
  straight(Sorted, [V1,V2,V3,V4,V5], _).

%four_of_a_kind(Hand+, Cardvalue-, value-)
four_of_a_kind([card(_,V1), card(_,V1), card(_,V1), card(_,V1)|_], V1, 2). %four cards of same value
four_of_a_kind([card(_,_)|R], V1, 2) :-
  four_of_a_kind(R, V1, 2).

%full_house(Hand+, Cardvalues-, value-)
full_house([card(_,V1), card(_,V1), card(_,V1)|R], [V1,V1,V1,V2,V2], 3) :- %three of a kind in the first cards
  pair(R, V2, _). %must be a pair in the rest to have a full house
full_house([card(_,V2), card(_,V2)|R], [V1,V1,V1,V2,V2], 3) :-  %pair in the first cards
  three_of_a_kind(R, V1, _).  %must be three_of_a_kind in the rest to have a full house
full_house([card(_,_)|R], V1, 3) :- %checks the upcoming cards
  full_house(R, V1, 3).

%flush(Hand+, Flushcardvalues-, value-)
flush([card(X, V1), card(X, V2), card(X, V3), card(X, V4), card(X, V5)|_], [V1,V2,V3,V4,V5], 4).
flush([card(_,_)|R], X, 4) :- %checks the rest of the hand
  flush(R, X, 4).

%straight(Hand+, straightcardvalues-, value-)
straight([card(_, V1), card(_, V2), card(_, V3), card(_, V4), card(_, V5)|_], [V1,V2,V3,V4,V5], 5) :-
  X is V1-V5,
  X == 4. %when we got a straight there is 4 steps between highest and lowest in the straight
straight([card(C,14)|R], X, 5) :- %if one of the cards is an ace we need to check straight ace to 5
  append(R, [card(C,1)], L), %adds a temporary card in last place of the hand
  straight(L, X, 5).
straight([card(_,_)|R], X, 5) :-  %is true if there is a straight in the next cards
  straight(R, X, 5).

%three_of_a_kind(Hand+, Cardvalue-, value-)
three_of_a_kind([card(_,V1), card(_,V1), card(_,V1)|_], V1, 6).
three_of_a_kind([card(_,_)|R], V1, 6) :-
  three_of_a_kind(R, V1, 6).

%two_pair(Hand+, cardvalues-, value-), uses pair
two_pair(Hand, [V1,V2], 7) :-
findall(X,pair(Hand,X,_),[V1,V2|_]). %finds all pairs, we use the 2 highest

%pair(Hand+, Cardvalue-, value-)
pair([card(_,V1), card(_,V1)|_], V1, 8).
pair([card(_,_)|R], V1, 8) :-
  pair(R, V1, 8).

%nothing(Hand+, Bestcards-, value-), to check one players card
nothing([card(_,V1),card(_,V2),card(_,V3),card(_,V4),card(_,V5)|_], FiveBest, 9) :-
  FiveBest = [V1,V2,V3,V4,V5], !.

%highest_card(Hand1+, Hand2+, Highestcard-)
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

/*Remove an all occurences of Value from
  a list and returning it in Res */
remove([], _, []):- !.
remove([card(_, Value)|T], Value, Res):-
  remove(T, Value, Res), !.
remove([card(_, Wrong)|T], Value, [Wrong|Res]):-
  remove(T, Value, Res), !.

/*Removes all occurences of Value1 and Value2 from
  a list and returning the new list in Res */
remove([],_, _, []):- !.
remove([card(_, Value1)|T], Value1, Value2, Res):-
  remove(T, Value1,Value2, Res),!.
remove([card(_, Value2)|T], Value1, Value2, Res):-
    remove(T, Value1,Value2, Res),!.
remove([card(_, Wrong)|T], Value1, Value2, [Wrong|Res]):-
  remove(T, Value1, Value2, Res),!.

/*Removes cards that occur with the same value
  more than once, used to check for straight */
doubleRemove([], []).
doubleRemove([card(Color1, Value),card(_, Value)|T], L):-
  !, doubleRemove([card(Color1, Value)|T], L).
doubleRemove([card(Color, Value)|T], [card(Color, Value)|L]):-
  doubleRemove(T, L).
