:-module(aiLogic, [whatToDo_preFlop/2,whatToDo_Flop/2,
                    whatToDo_Turn/2, whatToDo_River/2]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(library(random)).

/*Determines how the AI acts based
  on it's starting hand */
whatToDo_preFlop(PlayerAction, Answer):-
  player2(X),
  value_preflop(X, NewValue), % The hand is evaluated here
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

whatToDo_Flop(PlayerAction, Answer) :-
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

whatToDo_Turn(PlayerAction, Answer) :-
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
    ; PlayerAction == check, Result > 10 -> Answer = ai_check
    ; PlayerAction == bet, Result > 10 -> Answer = ai_call
    ; PlayerAction == raise, Result > 10 -> Answer = ai_call
    ; PlayerAction == check -> Answer = ai_check
    ; PlayerAction == bet -> Answer = ai_fold
    ; PlayerAction == raise -> Answer = ai_fold
    ).


%  .

whatToDo_River(PlayerAction, Answer) :-
    player2([A,B]),
    flop([C,D,E]),
    turn([F]),
    river([G]),
    random(0,25,Humanfactor),
    OurHand = [A,B,C,D,E,F,G],
    SharedHand = [C,D,E,F,G],
    check(SharedHand, FiveBest1, HV1),  % We check if our best hand of a total 5 cards is only by using the 5 shared
    check(OurHand, FiveBest2, HV2),     % cards or in combination with our own hand consisting of 7 cards
    pairevaluator(FiveBest2, R),
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
      ; PlayerAction == raise, HV1 == 5 -> Answer = ai_call
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
      ; PlayerAction == bet -> Answer = ai_call
      ; PlayerAction == raise -> Answer = ai_call
      ).


%Evaluates three of a kind
pairevaluator([V1,V1,V1,V4,V5], Result):-
  player2([card(_,A),card(_,B)]),
  max(A, B, C),
  ( V1 == A, V1 == B -> Result is 200             %Three of a kind with pocketpair
  ; (V1 == A ; V1 == B) -> Result is V1*V1 + 50   %Three of a kind with a card from our hand
  ; (V4 == C ; V5 == C), C > 11 -> Result is C*2  %Shared three of a kind but having a high kicker
  ; Result is 0
  ), !.


%Evaluates two pair
pairevaluator([V1,V1,V3,V3,V5], Result):-
  player2([card(_, A),card(_, B)]),
  (   V1 == A, V1 == B -> Result is V1 * V1 * 2                         %Two pair with the high pocketpair
    ; V3 == A, V3 == B -> Result is V3 * V3 + 70                        %Two pair with the low pocketpair
    ; (V1 == A, V3 == B ; V3 == A, V1 == B) -> Result is V1 * V3 * 2.5  %Two pair with both cards making one pair each
    ; (V1 == A ; V1 == B) -> Result is V1 * V1 * 1.5                    %Two pair with the high card connected
    ; (V3 == A ; V3 == B) -> Result is V3 * V3 * 0.5                    %Two pair with the low card connected
    ; (V5 == A ; V5 == B), V5 > 11 -> Result is V5                      %Two pair with shared hand but a high kicker
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
  player2([card(_, A),card(_, B)]),
  (   (A > 11 ; B > 11) -> Result is 5  %If we have a card Queen or higher
    ; Result is 0
    ).

%Calculates the chance of getting a straight, flush or both
chance(Hand, Value) :-
  sortByNumber(Hand, Sorted),
  sortByColor(Sorted, ColorSorted),
  doubleRemove(Sorted, Res),
  (   flush_Chance(ColorSorted, X), straight_Chance(Res, Y) ->  Value is X*Y+20  %Both Flush and Straight chance
    ; flush_Chance(ColorSorted, X) ->  Value is (X+5)*2                          %Flush chance
    ; straight_Chance(Sorted, Y) -> Value is Y+5                                 %Straight chance
    ; Value is 0
    ).

%Grants a bonus if there are 4 cards of the same color
flush_Chance([card(X, Y),card(X, _),card(X, _),card(X, _)|_], Y).
flush_Chance([card(_,_)|R], Y) :-
  flush_Chance(R, Y).

%Grants a bonus if there are 4 cards which gives a possible straight chance
straight_Chance([card(_,A),card(_,_),card(_,_),card(_,D)|_], A) :-
  X is A - D,
  (X == 3 ; X == 4).
straight_Chance([card(C,14)|R], Y) :- %Counting Ace as a possible straight with low cards aswell
  append(R, [card(C,1)], L),
  straight_Chance(L, Y).
straight_Chance([card(_,_)|R], Y) :-
  straight_Chance(R, Y).

/*Evaluates the hand pre_flop and returns a value
which the bot bases it's first decision on */
value_preflop([card(_, X),card(_, X)], Y):- % Cases where we have a pocketpair
  pokertable([_, _, _, Last_to_Act, _]), % We want to know Last_to_Act because we need to know if we are big or small blind. Being small blind grants a lower value
  random(-75, 75, Humanfactor), % Calculated variable to make the bot harder to read.
 (  Last_to_Act == ai -> Y is X*30 + 40 + Humanfactor
  ;  Y is X * 30 + Humanfactor ).

value_preflop([card(Color1, V1),card(Color2, V2)], Y):-
  pokertable([_, _, _, Last_to_Act, _]),
  absolut(V1, V2, Res), StraightChanceBonus is 20 - Res * Res, % Gives points if the cards have a chance of making a straight
  HandValue is V1 * V2,
  max(V1, V2, Hi), HighCardBonus is Hi * 2, % THe highest card in your starter hand grants bonus value
  random(-75, 75, Humanfactor),
  (  Last_to_Act == ai, Res < 5 -> Z is HandValue + StraightChanceBonus + HighCardBonus
  ;  Last_to_Act == player, Res < 5 -> Z is HandValue + StraightChanceBonus + HighCardBonus - 30
  ;  Last_to_Act == ai -> Z is HandValue + HighCardBonus + 30
  ;  Last_to_Act == player -> Z is HandValue + HighCardBonus - 30 ),
  (  Color1 == Color2 -> Y is Z + 30 + Humanfactor % If the cards are the same color we grant a 30 value bonus
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
