
:-module(game,[game/5]).
:-use_module(dealer).

% game(-, -, -, -, -). 
game(P1,P2,Flop,Turn,River):-
  deck(Shuffled),
  dealtp(Shuffled, P1 , P2, Restfromtp),
  dealflop(Restfromtp, Flop, Restfromflop),
  dealturn(Restfromflop, Turn, Restfromturn),
  dealriver(Restfromturn, River).
