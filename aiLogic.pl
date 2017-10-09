:-module(aiLogic, [between/3, value_preflop/2, whatToDo_preFlop/3,
                  whatToDo_Flop/3, whatToDo_Turn/3, whatToDo_River/3,
                  chance_For_Straight/1, chance_For_Flush/1, chance/2]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(library(random)).

/*Determines how the AI acts based
  on it's starting hand */
whatToDo_preFlop(Y, PlayerAction, Answer):-
  player2(X),
  value_preflop(X, Value),
  NewValue is Value - Y * 50,
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

whatToDo_Flop(Y, PlayerAction, Answer) :-
  player2([A,B]),
  flop([C,D,E]),
  Hand = [A,B,C,D,E],
  chance(Hand, Result),
  check(Hand, _, V),
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

%whatToDo_Turn(Y, PlayerAction, Answer) :-

%  .

%whatToDo_River(Y, PlayerAction, Answer) :-

%  .

chance(Hand, Value) :-
  sortByNumber(Hand, Sorted),
  sortByColor(Sorted, ColorSorted),
  (   chance_For_Flush(ColorSorted, X), chance_For_Straight(Sorted, Y) ->  Value is X*Y+20
    ; chance_For_Flush(ColorSorted, X) ->  Value is (X+5)*2
    ; chance_For_Straight(Sorted, Y) -> Value is Y
    ; Value is 0
    ).


chance_For_Flush([card(X, Y),card(X, _),card(X, _),card(X, _)|_], Y).
chance_For_Flush([card(_,_)|R], Y) :-
  chance_For_Flush(R, Y).


chance_For_Straight([card(_,A),card(_,B),card(_,C),card(_,D)|_], A) :-
  X is A - B + B - C + C - D,
  (X == 3 ; X == 4).
chance_For_Straight([card(C,14)|R], Y) :-
  append(R, [card(C,1)], L),
  chance_For_Straight(L, Y).
chance_For_Straight([card(_,_)|R], Y) :-
  chance_For_Straight(R, Y).

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
absolut(X, Y, Z):-
  X < Y,
  Z is Y - X, !.
absolut(X, Y, Z):-
  Z is X - Y.
absolut(14, 2, 1).
absolut(2, 14, 1).

%True if X is between Y and Z
between(X, Y, Z):-
  Y < X, X < Z.

%Returns the max value of X and Y and returns in Z
max(X, Y, Y):-
  X < Y, !.
max(X, _, X).
