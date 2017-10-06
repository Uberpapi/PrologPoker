:-module(printtable, [pt/0]).
:-use_module(dealer).

pt :-
pokertable([Stack, Pot, [B1,B2], _]),
Aistack is 2000-Stack,
player1(P1),
player2(P2),
(flop(Flop) -> flop(Flop) ; Flop = ''),
(turn(Turn) -> turn(Turn) ; Turn = ''),
(river(River) -> river(River) ; River = ''),
format('          ~`#t~53|~n', []),
format('        ##~t~t##~55|~n', []),
format('      ##~t~t##~57|~n', []),
format('     ##~t~t##~58|~n', []),
format('    ##~t~t##~59|~n', []),
format('   ##~t~t##~60|~n', []),
format('  ##~t~t##~61|~n', []),
format('  ##~t~t##~61|~n', []),
format('  ##~t~w~20|~w~w##~61|~n', [Flop, Turn, River]),    %~w betyder att output kan vara av vilken typ som helst, ~25| betyder att det försöker alignas till vänster om en brytpunkt 25 tecken in
format('  ##~t~t##~61|~n', []),
format('  ##~t~t##~61|~n', []),
format('   ##~t~t##~60|~n', []),
format('    ##~t~t##~59|~n', []),
format('     ##~t~t##~58|~n', []),
format('      ##~t~t##~57|~n', []),
format('        ##~t~w~43|~t##~55|~n', [P1]),  %~t = tab, ~w =  nästa arguments typ, ~43| =  ska vara en brytpunkt, ~n = newline
format('          ~`#t~53|~n', []).
