-module(blackjack).
-export([run/0]).


run() ->
	ShoeFileName = "SixDeck.txt",
	Strategies = strategy:basicStrategies(),
	InitialShoe = getShoeFromFile(ShoeFileName),
	HouseRules = {houseRules, true, true, 0.6, 10, 4},
	NumberOfDeals = 10,
	TrialResult = deal(NumberOfDeals, NumberOfDeals, HouseRules,
						Strategies, shoe:shuffle(InitialShoe), 1),
	TrialResult.



deal(0, _NumberOfDeals, _HouseRules, _Strategies, _Shoe, _ShoeLocation) ->
	{ok};

deal(DealsRemaining, NumberOfDeals, HouseRules, Strategies, Shoe, ShoeLocation) ->
	DealResult = engine:nextDeal({dealContext, HouseRules, Strategies, Shoe, ShoeLocation}),
	{dealData, Hands, Actions, Wagers, Payouts, CardsDealt} = DealResult,
	io:format("~w:~p~p~w~w~n", [(NumberOfDeals - DealsRemaining + 1), Hands, Actions, Wagers, Payouts]),
	case (ShoeLocation + CardsDealt) / length(Shoe) >= houserules:shufflePoint(HouseRules) of
		true ->
			deal(DealsRemaining - 1, NumberOfDeals, HouseRules, Strategies, shoe:shuffle(Shoe), 1);
		false ->
			deal(DealsRemaining - 1, NumberOfDeals, HouseRules, Strategies, Shoe, ShoeLocation + CardsDealt)
	end.


getShoeFromFile(Filename) ->
	{ok, FH} = file:open(Filename, read),
	getShoeFromFile(FH, "").

getShoeFromFile(FH, Contents) ->
	Line = io:get_line(FH, ''),
	case Line of
		eof -> Contents;
		_ -> getShoeFromFile(FH, Contents ++ lists:sublist(Line, length(Line) -1))
	end.
