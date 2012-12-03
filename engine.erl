-module(engine).
-export([nextDeal/1, test/0]).


nextDeal(DealContext) ->
	{dealResult, HandsPlay, ActionsPlay, CardsDealt} = play(start, DealContext, [], [], 0, 0),
	Hands = lists:map(fun(X) -> lists:reverse(X) end, lists:reverse(HandsPlay)),
	Actions = lists:map(fun(X) -> lists:reverse(X) end, lists:reverse(ActionsPlay)),
	{dealContext, HouseRules, _PlayerStrategy, _Shoe, _StartingShoeLocation} = DealContext,
	case Actions =:= [] of
		true ->
			case evaluate:score(lists:last(Hands)) =:= 21 of
				true -> {dealData, Hands, Actions, [houserules:baseWager(HouseRules)],
								[houserules:baseWager(HouseRules)], CardsDealt};
				false -> {dealData, Hands, Actions, [houserules:baseWager(HouseRules)],
								[0], CardsDealt}
			end;
		false ->
			case (length(Hands) =:= 2) and (evaluate:score(lists:last(Hands)) =:= 21) and 
								(length(lists:last(Hands)) =:= 2) of
				true ->
					{dealData, Hands, Actions, [houserules:baseWager(HouseRules)], 
								[(houserules:baseWager(HouseRules) * 5) div 2], CardsDealt};
				false ->
					DealerScore = evaluate:score(hd(Hands)),
					IsDouble = evaluate:isDouble(hd(Actions)),
					Wager = case IsDouble of
							true -> houserules:baseWager(HouseRules) * 2;
							false -> houserules:baseWager(HouseRules)
						end,
					{WagersOutcome, PayoutsOutcome} = money(DealerScore, Wager, tl(Hands), Actions, {[], []}),
					Wagers = lists:reverse(WagersOutcome),
					Payouts = lists:reverse(PayoutsOutcome),
					{dealData, Hands, Actions, Wagers, Payouts, CardsDealt}
			end
	end.



money(_DealerScore, _Wager, [], _Actions, {Wagers, Payouts}) ->
	{Wagers, Payouts};

money(DealerScore, Wager, PlayerHands, Actions, {Wagers, Payouts}) ->
	PlayerScore = evaluate:score(hd(PlayerHands)),
	if
		PlayerScore > 21 ->
			money(DealerScore, Wager, tl(PlayerHands), tl(Actions), {[Wager | Wagers], [0 | Payouts]});
		DealerScore > 21 ->
			money(DealerScore, Wager, tl(PlayerHands), tl(Actions), {[Wager | Wagers], [(2 * Wager) | Payouts]});
		PlayerScore > DealerScore ->
			money(DealerScore, Wager, tl(PlayerHands), tl(Actions), {[Wager | Wagers], [(2 * Wager) | Payouts]});
		DealerScore > PlayerScore ->
			money(DealerScore, Wager, tl(PlayerHands), tl(Actions), {[Wager | Wagers], [0 | Payouts]});
		DealerScore =:= PlayerScore ->
			money(DealerScore, Wager, tl(PlayerHands), tl(Actions), {[Wager | Wagers], [Wager | Payouts]})
	end.



play(start, DealContext, Hands, Actions, CardsDealt, _DealerShow) ->
	{dealContext, _HouseRules, _PlayerStrategy, Shoe, StartingShoeLocation} = DealContext,
	PlayerCards = [shoe:nextCard(Shoe, StartingShoeLocation + 2), 
						shoe:nextCard(Shoe, StartingShoeLocation) | []],
	DealerCards = [shoe:nextCard(Shoe, StartingShoeLocation + 3), 
						shoe:nextCard(Shoe, StartingShoeLocation + 1) | []],
	case evaluate:score(DealerCards) of
		21 -> play(finished, DealContext, [PlayerCards, DealerCards | Hands], Actions, 
						CardsDealt + 4, hd(tl(DealerCards)));
		_ -> play(playerAction, DealContext, [PlayerCards, DealerCards | Hands], ["" | Actions], 
						CardsDealt + 4, hd(tl(DealerCards)))
	end;

play(playerAction, DealContext, Hands, Actions, CardsDealt, DealerShow) ->
	{dealContext, HouseRules, PlayerStrategy, Shoe, StartingShoeLocation} = DealContext,
	PendingHands = length(Hands) - length(Actions) - 1,
	case evaluate:score(hd(lists:nthtail(PendingHands, Hands))) > 21 of
		true ->
			play(nextHand, DealContext, Hands, Actions, CardsDealt, DealerShow);
		false ->
			Context = evaluate:context([DealerShow], hd(lists:nthtail(PendingHands, Hands)), length(Hands),
							houserules:doubleAfterSplit(HouseRules), houserules:maxHands(HouseRules)),
			Action = strategy:act(PlayerStrategy, Context),
			ActionSet = [ Action | hd(Actions)],
			case Action of
				$S -> 
					play(nextHand, DealContext, Hands, [ActionSet | tl(Actions)], CardsDealt, DealerShow);
				$D -> 
					PlayerCards = [shoe:nextCard(Shoe, StartingShoeLocation + CardsDealt) |
							hd(lists:nthtail(PendingHands, Hands))],
					play(nextHand, DealContext, lists:sublist(Hands, PendingHands) ++ 
							[PlayerCards | tl(lists:nthtail(PendingHands, Hands))], 
							[ActionSet | tl(Actions)], CardsDealt + 1, DealerShow);
				$P ->
					PlayerCards = [shoe:nextCard(Shoe, StartingShoeLocation + CardsDealt) | 
						[hd(hd(lists:nthtail(PendingHands, Hands)))]],
					NextCards = [shoe:nextCard(Shoe, StartingShoeLocation + CardsDealt + 1) | 
						tl(hd(lists:nthtail(PendingHands, Hands)))],
					play(playerAction, DealContext, [NextCards | lists:sublist(Hands, PendingHands)] ++ 
							[PlayerCards | tl(lists:nthtail(PendingHands, Hands))],
							[ActionSet | tl(Actions)], CardsDealt + 2, DealerShow);
				$H ->
					PlayerCards = [shoe:nextCard(Shoe, StartingShoeLocation + CardsDealt) | 
							hd(lists:nthtail(PendingHands, Hands))],
					play(playerAction, DealContext, lists:sublist(Hands, PendingHands) ++
							[PlayerCards | tl(lists:nthtail(PendingHands, Hands))],
							[ActionSet | tl(Actions)], CardsDealt + 1, DealerShow)
			end
	end;

play(nextHand, DealContext, Hands, Actions, CardsDealt, DealerShow) ->
	case length(Hands) - 1 =:= length(Actions) of
		true -> play(dealerDraw, DealContext, Hands, Actions, CardsDealt, DealerShow);
		false -> play(playerAction, DealContext, Hands, ["" | Actions], CardsDealt, DealerShow)
	end;

play(dealerDraw, DealContext, Hands, Actions, CardsDealt, DealerShow) ->
	{dealContext, HouseRules, _PlayerStrategy, Shoe, StartingShoeLocation} = DealContext,
	DealerScore = evaluate:score(lists:last(Hands)),
	DealerNeedsCard = (DealerScore < 17) or ((DealerScore =:= 17) and (not houserules:standAny17(HouseRules)) and
							(evaluate:isSoft(lists:last(Hands)))),
	case DealerNeedsCard of
		true -> 
			DealerCards = [shoe:nextCard(Shoe, StartingShoeLocation + CardsDealt) | lists:last(Hands)],
			play(dealerDraw, DealContext, lists:sublist(Hands, length(Hands) - 1) ++ [DealerCards], 
								Actions, CardsDealt + 1, DealerShow);
		false ->
			play(finished, DealContext, Hands, Actions, CardsDealt, DealerShow)
	end;

play(finished, _DealContext, Hands, Actions, CardsDealt, _DealerShow) ->
	{dealResult, Hands, Actions, CardsDealt}.





test() ->
	{ok}.