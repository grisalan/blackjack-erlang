-module(evaluate).
-export([score/1, context/5, isSoft/1, isDouble/1, test/0]).


score(Hand) -> 
	{AcePresent, HandCount} = handEval(Hand),
	case (not AcePresent) or (HandCount > 11) of
		true -> HandCount;
		false -> HandCount + 10
	end.


context(DealerCard, PlayerCards, NextSplitPosition, DoubleAfterSplit, MaxHands) ->
	DoublePossible = case (((not(NextSplitPosition > 2)) or DoubleAfterSplit)
							and (length(PlayerCards) =:= 2)) of
		true -> "Y";
		false -> "N"
	end,
	SplitPossible = case (length(PlayerCards) =:= 2) and (hd(PlayerCards) =:= hd(tl(PlayerCards)))
							and (NextSplitPosition =< MaxHands) of
		true -> "Y";
		false -> "N"
	end,
	playerScoreStr(PlayerCards) ++ DealerCard ++ DoublePossible ++ SplitPossible.


isSoft(Hand) ->
	{AcePresent, HandCount} = handEval(Hand),
	not ((not AcePresent) or (HandCount > 11)).


isDouble(Actions) -> lists:any(fun(A) -> A =:= $D end, Actions).




stringifiedHandCount(HandCount) ->
	case HandCount > 9 of
		true -> integer_to_list(HandCount);
		false -> "0" ++ integer_to_list(HandCount)
	end.


playerScoreStr(PlayerCards) ->
	{AcePresent, HandCount} = handEval(PlayerCards),
	case (not AcePresent) or (HandCount > 11) of
		true -> stringifiedHandCount(HandCount) ++ "H";
		false -> stringifiedHandCount(HandCount + 10) ++ "S"
	end.


handEval(Hand) ->
	AcePresent = lists:any(fun(H) -> H =:= $A end, Hand),
	HandCount = lists:foldl(fun(H, Sum) -> cardCount(H) + Sum end, 0, Hand),
	{AcePresent, HandCount}.
	

cardCount($A) -> 1;
cardCount($T) -> 10;
cardCount($9) -> 9;
cardCount($8) -> 8;
cardCount($7) -> 7;
cardCount($6) -> 6;
cardCount($5) -> 5;
cardCount($4) -> 4;
cardCount($3) -> 3;
cardCount($2) -> 2.


test() ->
	17 = score("A6"),
	16 = score("T5A"),
	true = isSoft("A7"),
	false = isSoft("TA7"),
	false = isDouble(""),
	true = isDouble("PPD"),
	"21S7YN" = context("7", "AT", 2, true, 4),
	{ok}.
