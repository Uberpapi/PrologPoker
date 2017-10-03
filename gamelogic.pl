:-module(gamelogic, ).
:-use_module(game).

high_card(Cards)
pair(Cards) :-
two_pair(Cards);
three_of_a_kind(Cards) ->
four_of_a_kind(Cards);
straight(Cards) ->
flush(Cards);

successor(straight_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).
successor(full_house, flush).
successor(flush, straight).
successor(straight, three_of_a_kind).
successor(three_of_a_kind, two_pair).
successor(two_pair, pair).
successor(pair, highest_card).

successor('A', 'K').
successor('K', 'Q').
successor('Q','J').
successor('J',10).
successor(10, 9).
successor(9, 8).
successor(8, 7).
successor(7,6).
successor(6, 5).
successor(5, 4).
successor(4, 3).
successor(3, 2).
successor(2, 1).
