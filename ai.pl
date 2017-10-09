
:-module(ai, [ai_magic/1]).
:-use_module(dealer).
:-use_module(aiLogic).
:-use_module(pokerrules).

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

pre_flop :-
  pokertable([_, _, _, Last_to_Act ,_]),
  (   Last_to_Act == player -> ai_call
    ; nl, write('Do you want to call, raise or fold?')), nl.

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
  pokertable([Stack, Pot, [B1,B2], Last_to_Act, _]),
  (B1 > B2 -> X = B1, W = player
  ; X = B2, W = ai),
  Y is Stack - X,
  Z is Pot + X,
  P is Pot + 10,
  (   length(Deck, 48), Last_to_Act == ai -> setPokertable([Y, Z, [B1, B2], W, _]), dealflop(Deck), write('Flop is: '), flop
    ; length(Deck, 48), Last_to_Act == player -> A = 5, setPokertable([Stack, P, [B1, B2], Last_to_Act, _])
    ; length(Deck, 44), Last_to_Act == ai -> setPokertable([Y, Z, [B1, B2], W, _]), dealturn(Deck), write('Turn is: '), turn
    ; length(Deck, 42), Last_to_Act == ai -> setPokertable([Y, Z, [B1, B2], W, _]), dealriver(Deck), write('River is: '), river
    ; length(Deck, 40), Last_to_Act == ai -> setPokertable([Y, Z, [B1, B2], W, _]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2)
    ),
    (length(Deck, 40) -> ! ; W == player, A \== 5 -> ai_magic(check) ; nl, write('Do you want to check, bet or fold?'),nl),
    nl.

ai_bet :-
  nl,
  write('AI bet!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], _, _]),
  (B1 > B2 -> X = B1
  ; X = B2),
  Z is Pot + X,
  setPokertable([Stack, Z, [B1, B2], player, _]),
  write('Do you want to call, raise or fold?'),
  nl.

ai_raise :-
  deck(Deck),
  nl,
  write('AI raise!'),
  nl,
  pokertable([Stack, Pot, [B1,B2], _, _]),
  (B1 > B2 -> X = B1
  ; X = B2),
  Z is Pot + X*2,
  P is Pot + X*1.5,
  (length(Deck, 48) -> setPokertable([Stack, P, [B1, B2], player, _])
  ; setPokertable([Stack, Z, [B1, B2], player, _])),
  write('Do you want to call, raise or fold?'),
  nl.

ai_fold :-
  pokertable([Stack, Pot, _, _, _]),
  Newstack is Stack + Pot,
  setPokertable([Newstack, _, _, _, _]),
  write('AI Folds, you win '),
  write(Pot),
  write('$'),
  nl.
