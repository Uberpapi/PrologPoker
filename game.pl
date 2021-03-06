
:-module(game,[echo/0, go/0, allin/0]).
:-use_module(dealer).
:-use_module(pokerrules).
:-use_module(ai).
:-use_module(aiLogic).
:-use_module(printtable).

%Unionize for easier prints
flop :- flop(X), write(X).
turn :- turn(X), write(X).
river :- river(X), write(X).

%Read next input from stream, checks if it's a valid command and if it is call with it
echo :-
  write('>> '),
  read(X),
  (X == end_of_file  -> nl, write('Exiting echo'),!
  ;accepted_commands(X) -> call(X), echo
  ;format('~w is not a valid command.~nThe valid commands are:~nplay - Restart the game~ngo   - Deals a new hand~nAnd all the poker actions~n', [X]), echo).

%States the accepted commands
accepted_commands(X) :-
  (X == play; X== go; X == check; X == call; X == bet; X == raise; X == fold).

%Retract and set all the dynamic predicates and print some information
play :-
  retractall(flop(_)),
  retractall(turn(_)),
  retractall(river(_)),
  retractall(player1(_)),
  retractall(player2(_)),
  setPokertable([1000, 0, [25,50], ai, 0]),nl,
  write('Hello and welcome to this uber good poker game'), nl,
  write('You start with a stack of 1000'), nl,
  write('The different commands is "go", "check", "bet", "call" or "fold"'), nl,
  write('To deal a hand write "go"'),nl,
  pt.

%Retract the nessesary predicates, create and shuffle a new deck and then deal cards to players.
go :-
  retractall(flop(_)),
  retractall(turn(_)),
  retractall(river(_)),
  pokertable([Stack, _, [B1,B2], Last_to_Act, Handsplayed]),
  createDeck(X),
  dealtp(X), %Deal 2 cards to the players
  (B1 > B2 -> W = ai %Checks who starts to act
  ; W = player),
  Newhandsplayed is Handsplayed + 1,%Increase handsplayed so we know when to raise the blinds
  IncBlind is Newhandsplayed mod 3,%When we got a multiple of 3 we want to raise the blinds
  (IncBlind = 0 -> NewB2 is B2*2, NewB1 is B1*2 %When IncBlind is 0 we want to double the blinds
  ; NewB2 is B2, NewB1 is B1), % If we did not increase the blinds, we want them to be the same as before
  Aistack is 2000-Stack, %Aistack will always be 2000-Stack
  (Last_to_Act = player ->
    (Stack > NewB2 -> Y is Stack-NewB2, Z is NewB1+NewB2 %Retract the correct blind from player stack
    ; Y = 0, Z = NewB1 + Stack) %If we cant afford the blind, take the whole stack as blind
  ; (Aistack > NewB1 ->  Y is Stack - NewB2, Z is NewB1+NewB2
    ;Y is Stack, Z is NewB1 + Stack)),
  setPokertable([Y, Z, [NewB2,NewB1], W, Newhandsplayed]),
  ( W == player -> ai_magic(pre) ; write('Do you want to call, raise or fold?'), nl),pt.

%When player checks we want to act differently depending on where we are in the hand and who's the last player to act
check :-
  deck(Deck),
  pokertable([_, _, _, Last_to_Act, _]),
  (   length(Deck, 48), Last_to_Act == player -> nl, dealflop(Deck), write('Flop is: '), flop, nl, ai_magic(check)
    ; length(Deck, 44), Last_to_Act == player ->  nl, dealturn(Deck), write('Turn is: '), turn, nl, ai_magic(check)
    ; length(Deck, 42), Last_to_Act == player ->  nl, dealriver(Deck), write('River is: '), river, nl, ai_magic(check)
    ; length(Deck, 40), Last_to_Act == player -> nl, player1Cards(X), player2Cards(Y), whoWon(X,Y)
    ; Last_to_Act == ai -> nl, ai_magic(check)
    ),
    pt,
    nl.

%PLayer bets depending on the blind, if the player cant afford the bet he goes all in
bet :-
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  (B1 > B2 -> X = B1
  ; X = B2),
  (Stack - X > 0 -> Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You bet ~d$!', [X]), ai_magic(bet)
  ;Y is 0, Z is Pot + Stack + Stack, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You dont have sufficient stack, so you went allin!~nThe AI called!~n', []), allin),
  pt,
  nl.

%Player raises depending on the blind, if the player cant afford the raise he goes all in
raise :-
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], _, Handsplayed]),
  ( B1 > B2 -> X = B1
    ; X = B2),
    Preflop is B1 + B2,
  (Stack - X > 0, length(Deck, 48), Pot == Preflop -> S is Stack - X*1.5, P is Pot + X*1.5, setPokertable([S, P, [B1, B2], ai, Handsplayed]), ai_magic(raise), pt
  ; Stack - X > 0 -> Y is Stack - X*2, Z is Pot + X*2, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You raise ~d$!', [X]), nl,  ai_magic(raise),pt
  ; Y is 0, Z is Pot + Stack + Stack, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You dont have sufficient stack, so you went allin!~nThe AI called!~n', []), allin).

%Player calls either the small blind or the AIs bet, depending on where we are in the hand we act differently
call :-
  deck(Deck),
  pokertable([Stack, Pot, [B1,B2], Last_to_Act, Handsplayed]),
  (B1 > B2 -> X = B1, W = player
    ; X = B2, W = ai),
    PrePot is B1 + B2,
    length(Deck, Cardsleft),
  (   Stack - X < 0 -> Y is 0, Z is Pot + Stack + Stack, setPokertable([Y, Z, [B1, B2], ai, Handsplayed]), format('You dont have sufficient stack, so you went allin!~nThe AI called!~n', []), allin
    ; Cardsleft == 48, PrePot == Pot  -> write('You call!'), nl, S is Stack - X/2, P is Pot + X/2, setPokertable([S, P, [B1, B2], Last_to_Act, Handsplayed])
    ; Cardsleft == 48 -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), dealflop(Deck), write('Flop is: '), flop, nl
    ; Cardsleft == 44 -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), dealturn(Deck), write('Turn is: '), turn, nl
    ; Cardsleft == 42 -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), dealriver(Deck), write('River is: '), river, nl
    ; Cardsleft == 40 -> write('You call!'), nl, Y is Stack - X, Z is Pot + X, setPokertable([Y, Z, [B1, B2], W, Handsplayed]), player1Cards(P1),player2Cards(P2), whoWon(P1,P2),pt),
  (  Cardsleft == 40 -> !
    ; W \== Last_to_Act, Stack - X > 0 -> nl, write('Do you want to check, bet or fold?'), nl, pt
    ; Stack - X > 0 -> nl, ai_magic(check), nl, pt
    ; !), nl.

%Deals what remains of the hand and then decide a winner
allin :-
  deck(Deck),
  (\+flop(_) ->  dealflop(Deck)),
  (\+turn(_) -> deck(Deckafterflop), dealturn(Deckafterflop)),
  (\+river(_) -> deck(Deckafterturn), dealriver(Deckafterturn)),
  player1Cards(P1),
  player2Cards(P2),
  pt,
  whoWon(P1,P2).

%Player folds
fold :-
  write('You lost the hand'), nl,
  pt, nl,
  write('Write "go." to deal the next hand'), nl.
