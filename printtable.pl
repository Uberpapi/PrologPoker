:-module(printtable, [pt/0]).
:-use_module(dealer).

valuetochar('J',11).
valuetochar('Q',12).
valuetochar('K',13).
valuetochar('A',14).

%Print that takes in two inputs, Who did What.
pt:-
pokertable([Stack, Pot, [B1,B2], _]),
Aistack is 2000-Stack-Pot,
(player1(_) -> player1(XP1), fixvalue(XP1,[card(PC1,PV1), card(PC2,PV2)]); PC1='*', PV1='*', PC2='*', PV2 ='*'),
(player2(_) -> player2(XP2), fixvalue(XP2,[card(AC1,AV1), card(AC2,AV2)]); AC1='*', AV1='*', AC2='*', AV2 ='*'),
(flop(_) -> flop(XF), fixvalue(XF, [card(FC1,FV1), card(FC2,FV2), card(FC3,FV3)]); FC1='*', FV1='*', FC2='*', FV2 ='*', FC3 ='*', FV3 ='*'),
(turn(_) -> turn(XT), fixvalue(XT, [card(TC1,TV1)]) ; TC1 = '*', TV1 = '*'),
(river(_) -> river(XR), fixvalue(XR, [card(RC1,RV1)]) ; RC1 = '*', RV1 = '*'),

format('          ~`#t~53|~n', []),
format('        ##    ~w$~t~w~36|~t##~55|~n', [Aistack,'---   ---']),
format('      ##~t~w~28|~w~t~2+~w~t~2+~w~t~2+~w~t##~57|~n', ['| ',AC1,'| | ',AC2,'|']),
format('     ##~t~w~28|~w~t~2+~w~t~2+~w~t~2+~w~t##~58|~n', ['| ',AV1,'| | ',AV2,'|']),
format('    ##~t~w~36|~t##~59|~n', ['---   ---']),
format('   ##~t##~60|~n', []),
format('  ##~t~w~45|~t##~61|~n', ['---   ---   ---   ---   ---']),
format('  ##~t~w~19|~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t##~61|~n', ['| ',FC1,'| | ',FC2,'| | ',FC3,'| | ',TC1,'| | ',RC1,'|']),
format('  ##~t~w~19|~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t~2+~w~t##~61|~n', ['| ',FV1,'| | ',FV2,'| | ',FV3,'| | ',TV1,'| | ',RV1,'|']),    %~w betyder att output kan vara av vilken typ som helst, ~25| betyder att det försöker alignas till vänster om en brytpunkt 25 tecken in
format('  ##~t~w~45|~t##~61|~n', ['---   ---   ---   ---   ---']),
format('  ##~t~w$~33|~t##~61|~n', [Pot]),
format('   ##~t##~60|~n', []),
format('    ##~t~w~36|~t##~59|~n', ['---   ---']),
format('     ##~t~w~28|~w~t~2+~w~t~2+~w~t~2+~w~t##~58|~n', ['| ',PC1,'| | ',PC2,'|']),
format('      ##~t~w~28|~w~t~2+~w~t~2+~w~t~2+~w~t##~57|~n', ['| ',PV1,'| | ',PV2,'|']),
format('        ##~t~w~36|~t~w$    ##~55|~n', ['---   ---',Stack]),  %~t = tab, ~w =  nästa arguments typ, ~43| =  ska vara en brytpunkt, ~n = newline
format('          ~`#t~53|~n', []).


fixvalue([],[]).
fixvalue([card(C,V)|T], [Card|Res]) :-
  (valuetochar(X,V) -> valuetochar(X,V), Card = card(C,X), fixvalue(T, Res)
  ;Card = card(C,V), fixvalue(T,Res)).
