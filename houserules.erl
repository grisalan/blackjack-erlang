-module(houserules).
-export([standAny17/1, doubleAfterSplit/1, shufflePoint/1, baseWager/1, maxHands/1, test/0]).


standAny17(HouseRules) ->
	{houseRules, StandAny17, _, _, _, _} = HouseRules,
	StandAny17.

doubleAfterSplit(HouseRules) ->
	{houseRules, _, DoubleAfterSplit, _, _, _} = HouseRules,
	DoubleAfterSplit.

shufflePoint(HouseRules) ->
	{houseRules, _, _, ShufflePoint, _, _} = HouseRules,
	ShufflePoint.

baseWager(HouseRules) ->
	{houseRules, _, _, _, BaseWager, _} = HouseRules,
	BaseWager.

maxHands(HouseRules) ->
	{houseRules, _, _, _, _, MaxHands} = HouseRules,
	MaxHands.



test() ->
	HouseRules = {houseRules, true, true, 0.75, 10, 4},
	true = standAny17(HouseRules),
	true = doubleAfterSplit(HouseRules),
	0.75 = shufflePoint(HouseRules),
	10 = baseWager(HouseRules),
	4 = maxHands(HouseRules),
	{ok}.
