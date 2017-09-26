
:-module(game,[play/0, done/0, go/0]).
:-use_module(dealer).

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
  dealtp(X, P1, P2, Y),
  dealflop(Y, Flop, W),
  dealturn(W, Turn, Z),
  dealriver(Z, River).
