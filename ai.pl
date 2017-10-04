:-module(ai, [ai_magic/1]).
:-use_module(dealer).

p1 :- listing(player1).
p2 :- listing(player2).
flop :- listing(flop).
turn :- listing(turn).
river :- listing(river).
playerstack :- listing(playerstack).

ai_magic(Player_Act) :-
  ( Player_Act == pre -> pre_flop
  ; Player_Act == check -> ai_check
  ; Player_Act == bet -> ai_call
  ; Player_Act == raise -> ai_call
  ; Player_Act == call -> ai_check
  ).

pre_flop :-
  %call/check pre flop
  pokertable([Stack, Pot, [B1, B2], Last_to_Act]),
  (   Last_to_Act == player -> ai_call
    ; !).

ai_check :-
  deck(Deck),
  pokertable([_, _, _, Last_to_Act]),
  (   length(Deck, 48), Last_to_Act == ai -> dealflop(Deck), flop, write('Do you want to check, bet or fold?')
    ; length(Deck, 48), Last_to_Act == player -> !
    ; length(Deck, 44), Last_to_Act == ai -> dealturn(Deck), turn, write('Do you want to check, bet or fold?')
    ; length(Deck, 44), Last_to_Act == player -> !
    ; length(Deck, 42), Last_to_Act == ai -> dealriver(Deck), river, write('Do you want to check, bet or fold?')
    ; length(Deck, 42), Last_to_Act == player -> !
    ; length(Deck, 40), Last_to_Act == ai -> write('Someone won')
    ; length(Deck, 40), Last_to_Act == player -> !
    ), nl,
    write('AI checks'), nl.

ai_call :-
  write('AI calls'), nl,
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], Last_to_Act]),
  (B1 > B2 -> X = B1, W = player
  ; X = B2, W = ai),
  Y is Stack - X,
  Z is Pot + X,
  P is Pot + 10,
  (   length(Deck, 48), Last_to_Act == ai -> setPokertable([Y, Z, [B1, B2], W]), dealflop(Deck), flop, write('Do you want to check, bet or fold?')
    ; length(Deck, 48), Last_to_Act == player -> setPokertable([Stack, P, [B1, B2], Last_to_Act]), write('Do you want to check, raise or fold?')
    ; length(Deck, 44) -> setPokertable([Y, Z, [B1, B2], W]), dealturn(Deck), turn,write('Do you want to check, bet or fold?')
    ; length(Deck, 42) -> setPokertable([Y, Z, [B1, B2], W]), dealriver(Deck), river, write('Do you want to check, bet or fold?')
    ; length(Deck, 40) -> setPokertable([Y, Z, [B1, B2], W]), write('Someone won')
    ),
    %INSERT AI MAGIC HERE
    nl.
