:-module(aiLogic, [between/3, value_preflop/2, whatToDo/3]).
:-use_module(dealer).
:-use_module(library(random)).

whatToDo(Y, PlayerAction, Answer):-
  player2(X),
  value_preflop(X, Value),
  print(Value), nl, nl,
  NewValue is Value - Y * 50,
  ( NewValue < 0, PlayerAction == pre -> Answer = ai_fold, print('1')
  ; NewValue < 0, PlayerAction == raise -> Answer = ai_fold, print('2')
  ; NewValue < 0, PlayerAction == bet -> Answer = ai_fold, print('3')
  ; NewValue < 50, PlayerAction == check -> Answer = ai_check, print('4')
  ; NewValue < 100, PlayerAction == pre -> Answer = ai_call, print('5')
  ; NewValue < 100, PlayerAction == raise -> Answer = ai_call, print('6')
  ; NewValue < 100, PlayerAction == bet -> Answer = ai_call, print('7')
  ; PlayerAction == bet -> Answer = ai_raise, print('8')
  ; PlayerAction == pre -> Answer = ai_raise, print('9')
  ; PlayerAction == check -> Answer = ai_bet, print('10')
  ; PlayerAction == raise -> Answer = ai_raise, print('11')
  ; PlayerAction == call -> Answer = ai_check), nl.


/*Evaluates the hand pre_flop
  and returns a value which the bot
  bases it's first decision on */
value_preflop([card(_, X),card(_, X)], Y):-
  pokertable([_, _, _, Last_to_Act]),
  random(-75, 75, Humanfactor),
 (  Last_to_Act == ai -> Y is X*30 + 40 + Humanfactor
  ;  Y is X * 30 + Humanfactor ).

value_preflop([card(Color1, V1),card(Color2, V2)], Y):-
  pokertable([_, _, _, Last_to_Act]),
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

max(X, Y, Y):-
  X < Y, !.
max(X, _, X).