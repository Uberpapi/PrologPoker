
:-module(ai, [ai_magic/1]).
:-use_module(dealer).
:-use_module(aiLogic).
:-use_module(pokerrules).
:-use_module(game).

%facts for printing table cards
flop :- flop(X), write(X).
turn :- turn(X), write(X).
river :- river(X), write(X).

/*ai_magic(Player_Act+)
Asks aiLogic what to do depending on what player did
and how many cards that have been dealt*/
ai_magic(Player_Act) :-
  deck(Deck),
  (   length(Deck, 48) -> whatToDo_preFlop(0, Player_Act, Answer)
    ; length(Deck, 44) -> whatToDo_Flop(0, Player_Act, Answer)
    ; length(Deck, 42) -> whatToDo_Turn(0, Player_Act, Answer)
    ; length(Deck, 40) -> whatToDo_River(0, Player_Act, Answer)
    ),
  call(Answer).

/*The ai checks and if ai is Last_to_Act cards are dealt
depending on which turn it is, otherwise asks the player
what it want to do*/
ai_check :-
  deck(Deck),
  nl,
  write('AI checks'),
  nl,
  pokertable([_, _, _, Last_to_Act, _]),
  (   length(Deck, 48), Last_to_Act == ai -> dealflop(Deck), write('Flop is: '), flop, write('Do you want to check, bet or fold?')
    ; length(Deck, 44), Last_to_Act == ai -> dealturn(Deck), write('Turn is: '), turn, write('Do you want to check, bet or fold?')
    ; length(Deck, 42), Last_to_Act == ai -> dealriver(Deck), write('River is: '), river, write('Do you want to check, bet or fold?')
    ; length(Deck, 40), Last_to_Act == ai -> player1Cards(X),player2Cards(Y), whoWon(X,Y) %if river is dealt we check who won
    ; Last_to_Act == player -> write('Do you want to check, bet or fold?')
    ),
    nl.

%The ai calls, actions are taken depending on which turn and who's Last_to_Act
ai_call :-
  nl,
  write('AI calls'),
  nl,
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], Last_to_Act, Handsplayed]),
  (B1 > B2 -> X = B1, Smallblind = B2, W = player %if B1 is big blind, player is the one acting last next time
  ; X = B2, Smallblind = B1, W = ai),
  Z is Pot + X, %calculate new pot if it's not the first act
  P is Pot + Smallblind, %calculate new pot if it's first act
  length(Deck, Cardsleft),
  (   Cardsleft == 48, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), dealflop(Deck), write('Flop is: '), flop
    ; Cardsleft == 48, Last_to_Act == player -> setPokertable([Stack, P, [B1, B2], Last_to_Act, Handsplayed]) %first act of the game
    ; Cardsleft == 44, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), dealturn(Deck), write('Turn is: '), turn
    ; Cardsleft == 42, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), dealriver(Deck), write('River is: '), river
    ; Cardsleft == 40, Last_to_Act == ai -> setPokertable([Stack, Z, [B1, B2], W, Handsplayed]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2)
    ),
  (   Cardsleft == 40 -> ! %should'nt do anything more if river is dealt
    ; Cardsleft \== 48, W == player -> ai_magic(check)  %checks if ai is next to act, and acts if true
    ; nl, write('Do you want to check, bet or fold?'), nl),
    nl.

%ai bets depending on big blind and if ai's got enough chips for a big blind
ai_bet :-
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  (B1 > B2 -> X = B1 %checks which is big blind (X)
  ; X = B2),
  Y is 2000-(Stack+Pot), %calculates how much chips ai got left
  ( Y < X -> Z is Pot+Y+Y, setPokertable([Stack, Z, [B1, B2], player, Handsplayed]), format('~nThe AI had ~d$ and went allin! ~nYou auto-called!~n', [Y]), allin
    ; Z is Pot + X, setPokertable([Stack, Z, [B1, B2], player, Handsplayed]), format('~nAI bet!~nDo you want to call, raise or fold?~n', [])
  ).

/*ai raises, calculates how much depending on if ai got enough chips and
if it's first act or not pot == prepot denotes that it's the first act*/
ai_raise :-
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  (B1 > B2 -> X = B1, Smallblind = B2
  ; X = B2, Smallblind = B1),
  Y is 2000-(Stack+Pot), %calculate how much ai got left.
  Raise is Y - Smallblind,  %calculates how much ai can raise at max, needed for when ai can't raise full big blind
  S is Stack - Raise, %calculates how much player calls when ai goes all in
  PrePot is B1+B2, %calculates how much the pot is at the moment
  Pre is X+X, %calculates how much is needed to be able to raise a full big blind
  (   Pot == PrePot, Y =< PrePot -> P is Pot+Y+Raise, setPokertable([S, P, [B1, B2], player, Handsplayed]), format('~nThe AI had ~d$ and went allin! ~nYou auto-called ~d$~n', [Y, Raise]), allin
    ; Pot == PrePot, Y >= Pre -> P is Pot+X+Smallblind, setPokertable([Stack, P, [B1, B2], player, Handsplayed]), format('~nAI raise!~nDo you want to call, raise or fold?~n', [])
    ; Y =< Pre -> P is Pot+Y+Y-X, setPokertable([S, P, [B1, B2], player, Handsplayed]), format('~nThe AI had ~d$ and went allin! ~nYou auto-called ~d$~n', [Y, Raise]), allin
    ; P is Pot + X*2, setPokertable([Stack, P, [B1,B2], player, Handsplayed]), format('~nAI raise!~nDo you want to call, raise or fold?~n', [])
  ).

%ai folds giving the pot to the player which is set in dynamic predicate pokertable
ai_fold :-
  pokertable([Stack, Pot, B1, B2, Handsplayed]),
  Newstack is Stack + Pot,
  setPokertable([Newstack, 0, B1, B2, Handsplayed]),
  format('~nAi folds, you win ~d$~nWrite "go" to deal a new hand~n', [Pot]).
