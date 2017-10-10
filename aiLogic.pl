:-module(aiLogic, [between/3, value_preflop/2, whatToDo_preFlop/3,
                  whatToDo_Flop/3, whatToDo_Turn/3, whatToDo_River/3,
                  straight_Chance/1, flush_Chance/1, chance/2]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(library(random)).

/*Determines how the AI acts based
  on it's starting hand */
whatToDo_preFlop(_, PlayerAction, Answer):-
  player2(X),
  value_preflop(X, NewValue),
  ( NewValue < 0, PlayerAction == pre -> Answer = ai_fold
  ; NewValue < 0, PlayerAction == raise -> Answer = ai_fold
  ; NewValue < 0, PlayerAction == bet -> Answer = ai_fold
  ; NewValue < 50, PlayerAction == check -> Answer = ai_check
  ; NewValue < 100, PlayerAction == pre -> Answer = ai_call
  ; NewValue < 100, PlayerAction == raise -> Answer = ai_call
  ; NewValue < 100, PlayerAction == bet -> Answer = ai_call
  ; PlayerAction == bet -> Answer = ai_raise
  ; PlayerAction == pre -> Answer = ai_raise
  ; PlayerAction == check -> Answer = ai_bet
  ; PlayerAction == raise -> Answer = ai_raise
  ; PlayerAction == call -> Answer = ai_check).

whatToDo_Flop(_, PlayerAction, Answer) :-
  player2([A,B]),
  flop([C,D,E]),
  Hand = [A,B,C,D,E],
  random(-5,10,Humanfactor),
  chance(Hand, R),
  check(Hand, X, V),
  pairevaluator(X, Res),
  Result is R + Res + Humanfactor,
  (   PlayerAction == check, V < 8 -> Answer = ai_bet
    ; PlayerAction == bet, V < 8 -> Answer = ai_raise
    ; PlayerAction == raise, V < 8 -> Answer = ai_raise
    ; PlayerAction == check, Result > 40 -> Answer = ai_bet
    ; PlayerAction == bet, Result > 40 -> Answer = ai_raise
    ; PlayerAction == raise, Result > 40 -> Answer = ai_call
    ; PlayerAction == check, Result > 20 -> Answer = ai_bet
    ; PlayerAction == bet, Result > 20 -> Answer = ai_call
    ; PlayerAction == raise, Result > 20 -> Answer = ai_call
    ; PlayerAction == check, Result > 0 -> Answer = ai_check
    ; PlayerAction == bet, Result > 0 -> Answer = ai_call
    ; PlayerAction == raise, Result > 0 -> Answer = ai_call
    ; PlayerAction == check -> Answer = ai_check
    ; PlayerAction == bet -> Answer = ai_fold
    ; PlayerAction == raise -> Answer = ai_fold
    ).

whatToDo_Turn(_, PlayerAction, Answer) :-
  player2([A,B]),
  flop([C,D,E]),
  turn([F]),
  Hand = [A,B,C,D,E,F],
  random(-5,10,Humanfactor),
  check(Hand, X, V),
  pairevaluator(X, Res),
  Result is Res + Humanfactor,

  (   PlayerAction == check, V < 7 -> Answer = ai_bet
    ; PlayerAction == bet, V < 7 -> Answer = ai_raise
    ; PlayerAction == raise, V < 7 -> Answer = ai_raise
    ; PlayerAction == check, Result > 115 -> Answer = ai_bet
    ; PlayerAction == bet, Result > 115 -> Answer = ai_raise
    ; PlayerAction == raise, Result > 115 -> Answer = ai_call
    ; PlayerAction == check, Result > 50 -> Answer = ai_bet
    ; PlayerAction == bet, Result > 50 -> Answer = ai_call
    ; PlayerAction == raise, Result > 50 -> Answer = ai_call
    ; PlayerAction == check, Result > 15 -> Answer = ai_check
    ; PlayerAction == bet, Result > 15 -> Answer = ai_call
    ; PlayerAction == raise, Result > 15 -> Answer = ai_call
    ; PlayerAction == check -> Answer = ai_check
    ; PlayerAction == bet -> Answer = ai_fold
    ; PlayerAction == raise -> Answer = ai_fold
    ).


%  .

whatToDo_River(_, PlayerAction, Answer) :-
    player2([A,B]),
    flop([C,D,E]),
    turn([F]),
    river([G]),
    random(0,25,Humanfactor),
    OurHand = [A,B,C,D,E,F,G],
    SharedHand = [C,D,E,F,G],
    check(SharedHand, FiveBest1, HV1),  % We check if our best hand of a total 5 cards is only by using the 5 shared
    check(OurHand, FiveBest2, HV2),     % cards or in combination with our own hand consisting of 7 cards
    pairevaluator(FiveBest, R),
    chance(SharedHand, SharedChance),
    Result is R - (SharedChance * 2) + Humanfactor,
    (   PlayerAction == check, HV1 \== HV2,  HV2 < 6 -> Answer = ai_bet %Cases where our hand is stronger than the shared hand
      ; PlayerAction == bet, HV1 \== HV2, HV2 < 6 -> Answer = ai_raise  %and our hand is consisting of a straight or better
      ; PlayerAction == raise, HV1 \== HV2, HV2 < 6 -> Answer = ai_raise
      ; PlayerAction == check, FiveBest1 \== FiveBest2,  HV2 < 6 -> Answer = ai_bet %Cases where our hand is the same as the shared, like
      ; PlayerAction == bet, FiveBest1 \== FiveBest2, HV2 < 6 -> Answer = ai_raise  %a straight for example but we have a higher straight
      ; PlayerAction == raise, FiveBest1 \== FiveBest2, HV2 < 6 -> Answer = ai_raise
      ; PlayerAction == check,  HV1 < 5 -> Answer = ai_bet %Cases where the shared hand is a flush or better
      ; PlayerAction == bet, HV1 < 5 -> Answer = ai_raise
      ; PlayerAction == raise, HV1 < 5 -> Answer = ai_call
      ; PlayerAction == check,  HV1 == 5, Humanfactor < 15 -> Answer = ai_bet %Cases where we have a shared straight
      ; PlayerAction == bet, HV1 == 5, Humanfactor < 15 -> Answer = ai_raise  %and we want to have a bluff variable
      ; PlayerAction == raise, HV1 == 5, Humanfactor < 15 -> Answer = ai_call
      ; PlayerAction == check,  HV1 == 5 -> Answer = ai_check %Also cases where it acts upon a shared straight
      ; PlayerAction == bet, HV1 == 5 -> Answer = ai_call     %but the Humanfactor is below 15 (out of 25)
      ; PlayerAction == raise, HV1 == 5 -> Answer = ai_fold
      ; PlayerAction == check,  Result > 115 -> Answer = ai_bet %The rest of the cases are based upon
      ; PlayerAction == bet, Result > 115 -> Answer = ai_raise  %if we have three of a kind or lower
      ; PlayerAction == raise, Result > 115 -> Answer = ai_call
      ; PlayerAction == check, Result > 50 -> Answer = ai_bet
      ; PlayerAction == bet, Result > 50 -> Answer = ai_call
      ; PlayerAction == raise, Result > 50 -> Answer = ai_call
      ; PlayerAction == check, Result > 10 -> Answer = ai_check
      ; PlayerAction == bet, Result > 10 -> Answer = ai_call
      ; PlayerAction == raise, Result > 10 -> Answer = ai_call
      ; PlayerAction == check -> Answer = ai_check
      ; PlayerAction == bet -> Answer = ai_fold
      ; PlayerAction == raise -> Answer = ai_fold
      ).


%Evaluates three of a kind
pairevaluator([V1,V1,V1,V4,V5], Result):-
  player2([card(_,A),card(_,B)]),
  max(A, B, C),
  ( V1 == A, V1 == B -> Result is 200    %Three of a kind with pocketpair
  ; (V1 == A ; V1 == B) -> Result is V1*V1 + 50   %Three of a kind with a card from our hand
  ; (V4 == C ; V5 == C), C > 11 -> Result is C*2  %Shared three of a kind but having a high kicker
  ; Result is 0
  ), !.


%Evaluates two pair
pairevaluator([V1,V1,V3,V3,V5], Result):-
  player2([card(_, A),card(_, B)]),
  (   V1 == A, V1 == B -> Result is V1 * V1 * 2
    ; V3 == A, V3 == B -> Result is V3 * V3 + 70
    ; V1 == A ; V1 == B -> Result is V1 * V1 * 1.5
    ; V3 == A ; V3 == B -> Result is V3 * V3 * 0.5
    ; (V1 == A, V3 == B ; V3 == A, V1 == B) -> Result is V1 * V3 * 2.5
    ; (V5 == A ; V5 == B), V5 > 11 -> Result is V5
    ; Result is 0
    ), !.

%Evaluates single pair
pairevaluator([V1,V1,_,_,V5], Result):-
  player2([card(_, A),card(_, B)]),
  (   V1 == A, V1 == B -> Result is V1*2              %pocketpair
    ; (V1 == A; V1 == B), V1 > V5 -> Result is V1 + 5 %pair with 1 card in hand
    ; (A > 11 ; B > 11) -> Result is 7                %pair on table high kicker on hand
    ; Result is 0                                     %else
  ), !.

%evaluates highest card
pairevaluator(_, Result):-
  player2([card(C1, A),card(C2, B)]),
  (   (A > 11 ; B > 11) -> Result is 5
    ; Result is 0
    ).

chance(Hand, Value) :-
  sortByNumber(Hand, Sorted),
  sortByColor(Sorted, ColorSorted),
  (   flush_Chance(ColorSorted, X), straight_Chance(Sorted, Y) ->  Value is X*Y+20
    ; flush_Chance(ColorSorted, X) ->  Value is (X+5)*2
    ; straight_Chance(Sorted, Y) -> Value is Y+5
    ; Value is 0
    ).


flush_Chance([card(X, Y),card(X, _),card(X, _),card(X, _)|_], Y).
flush_Chance([card(_,_)|R], Y) :-
  flush_Chance(R, Y).


straight_Chance([card(_,A),card(_,B),card(_,C),card(_,D)|_], A) :-
  X is A - B + B - C + C - D,
  (X == 3 ; X == 4).
straight_Chance([card(C,14)|R], Y) :-
  append(R, [card(C,1)], L),
  straight_Chance(L, Y).
straight_Chance([card(_,_)|R], Y) :-
  straight_Chance(R, Y).

/*Evaluates the hand pre_flop
  and returns a value which the bot
  bases it's first decision on */
value_preflop([card(_, X),card(_, X)], Y):-
  pokertable([_, _, _, Last_to_Act, _]),
  random(-75, 75, Humanfactor),
 (  Last_to_Act == ai -> Y is X*30 + 40 + Humanfactor
  ;  Y is X * 30 + Humanfactor ).

value_preflop([card(Color1, V1),card(Color2, V2)], Y):-
  pokertable([_, _, _, Last_to_Act, _]),
  absolut(V1, V2, Res), StraightChanceBonus is 20 - Res * Res,
  HandValue is V1 * V2,
  max(V1, V2, Hi), HighCardBonus is Hi * 2,
  random(-75, 75, Humanfactor),
  (  Last_to_Act == ai, Res < 5 -> Z is HandValue + StraightChanceBonus + HighCardBonus
  ;  Last_to_Act == player, Res < 5 -> Z is HandValue + StraightChanceBonus + HighCardBonus - 30
  ;  Last_to_Act == ai -> Z is HandValue + HighCardBonus + 30
  ;  Last_to_Act == player -> Z is HandValue + HighCardBonus - 30 ),
  (  Color1 == Color2 -> Y is Z + 30 + Humanfactor
  ; Y is Z + Humanfactor ).

%Returns the absolut value Z from X - Y
absolut(14, Y, Z):-
  Y < 6,
  absolut(1, Y, Z).
absolut(X, 14, Z):-
  X < 6,
  absolut(X, 1, Z).
absolut(X, Y, Z):-
  X < Y,
  Z is Y - X, !.
absolut(X, Y, Z):-
  Z is X - Y.

%Returns the max value of X and Y and returns in Z
max(X, Y, Y):-
  X < Y, !.
max(X, _, X).
