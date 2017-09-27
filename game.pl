
:-module(game,[play/0, done/0, go/0]).
:-use_module(dealer).
:-use_module(pokerrules)

p1 :- listing(player1).
p2 :- listing(player2).
flop :- listing(flop).
turn :- listing(turn).
river :- listing(river).

play :-
  write('>> '),
  read(X),
  call(X),
play.

go :-
  createDeck(X),
  dealtp(X, Y),
  dealflop(Y, W),
  dealturn(W, Z),
  dealriver(Z).
