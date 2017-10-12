
:-module(ai, [ai_magic/1]).
:-use_module(dealer).
:-use_module(aiLogic).
:-use_module(pokerrules).
:-use_module(game).

p1 :- player1(X), write(X).
p2 :- player2(X), write(X).
flop :- flop(X), write(X).
turn :- turn(X), write(X).
river :- river(X), write(X).
playerstack :- listing(playerstack).

ai_magic(Player_Act) :-
  deck(Deck),
  (   length(Deck, 48) -> whatToDo_preFlop(0, Player_Act, Answer)
    ; length(Deck, 44) -> whatToDo_Flop(0, Player_Act, Answer)
    ; length(Deck, 42) -> whatToDo_Turn(0, Player_Act, Answer)
    ; length(Deck, 40) -> whatToDo_River(0, Player_Act, Answer)
    ),
  call(Answer).

ai_check :-
  deck(Deck),
  nl,
  write('AI checks'),
  nl,
  pokertable([_, _, _, Last_to_Act, _]),
  (   length(Deck, 48), Last_to_Act == ai -> dealflop(Deck), write('Flop is: '), flop
    ; length(Deck, 44), Last_to_Act == ai -> dealturn(Deck), write('Turn is: '), turn, write('Do you want to check, bet or fold?')
    ; length(Deck, 42), Last_to_Act == ai -> dealriver(Deck), write('River is: '), river, write('Do you want to check, bet or fold?')
    ; length(Deck, 40), Last_to_Act == ai -> player1Cards(X),player2Cards(Y), whoWon(X,Y)
    ; Last_to_Act == player -> write('Do you want to check, bet or fold?')
    ),
    nl.

ai_call :-
  nl,
  write('AI calls'),
  nl,
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], Last_to_Act, Handsplayed]),
  (B1 > B2 -> X = B1, Smallblind = B2, W = player
  ; X = B2, Smallblind = B1, W = ai),
  Z is Pot + X,
  P is Pot + Smallblind,
  length(Deck, Cardsleft),
  (   Cardsleft == 48, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), dealflop(Deck), write('Flop is: '), flop
    ; Cardsleft == 48, Last_to_Act == player -> setPokertable([Stack, P, [B1, B2], Last_to_Act, Handsplayed])
    ; Cardsleft == 44, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), dealturn(Deck), write('Turn is: '), turn
    ; Cardsleft == 42, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), dealriver(Deck), write('River is: '), river
    ; Cardsleft == 40, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2)
    ),
  (   Cardsleft == 40 -> !
    ; Cardsleft \== 48, W == player -> ai_magic(check)
    ; nl, write('Do you want to check, bet or fold?'), nl),
    nl.

ai_bet :-
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  (B1 > B2 -> X = B1
  ; X = B2),
  Y is 2000-(Stack+Pot),
  ( Y < X -> Z is Pot+Y+Y, setPokertable([Stack, Z, [B1, B2], player, Handsplayed]), format('~nThe AI had ~d$ and went allin! ~nYou auto-called!~n', [Y]), allin
    ; Z is Pot + X, setPokertable([Stack, Z, [B1, B2], player, Handsplayed]), format('~nAI bet!~nDo you want to call, raise or fold?~n', [])
  ).

ai_raise :-
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  (B1 > B2 -> X = B1, Smallblind = B2
  ; X = B2, Smallblind = B1),
  Y is 2000-(Stack+Pot),
  Raise is Y - Smallblind,
  S is Stack - Raise,
  PrePot is B1+B2,
  Pre is X+X,
  (   Pot == PrePot, Y =< PrePot -> P is Pot+Y+Raise, setPokertable([S, P, [B1, B2], player, Handsplayed]), format('~nThe AI had ~d$ and went allin! ~nYou auto-called ~d$~n', [Y, Raise]), allin
    ; Pot == PrePot, Y >= Pre -> P is Pot+X+Smallblind, setPokertable([Stack, P, [B1, B2], player, Handsplayed]), format('~nAI raise!~nDo you want to call, raise or fold?~n', [])
    ; Y =< Pre -> P is Pot+Y+Y-X, setPokertable([S, P, [B1, B2], player, Handsplayed]), format('~nThe AI had ~d$ and went allin! ~nYou auto-called ~d$~n', [Y, Raise]), allin
    ; P is Pot + X*2, setPokertable([Stack, P, [B1,B2], player, Handsplayed]), format('~nAI raise!~nDo you want to call, raise or fold?~n', [])
  ).

ai_fold :-
  pokertable([Stack, Pot, B1, B2, Handsplayed]),
  Newstack is Stack + Pot,
  setPokertable([Newstack, 0, B1, B2, Handsplayed]),
  format('~nAi folds, you win ~d$~nWrite "go." to deal a new hand~n', [Pot]).
