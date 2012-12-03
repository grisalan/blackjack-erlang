-module(shoe).
-export([nextCard/2, shuffle/1, oneDeck/0, twoDeck/0, sixDeck/0, test/0]).


nextCard(Cards, Location) ->
	lists:nth(Location, Cards).


shuffle(Cards) ->
	shuffle:shuffle(Cards).


oneDeck() -> "AAAATTTTTTTTTTTTTTTT99998888777766665555444433332222".
twoDeck() -> oneDeck() ++ oneDeck().
sixDeck() -> twoDeck() ++ twoDeck() ++ twoDeck().


test() ->
	$4 = nextCard("AT67438T", 5),
	$A = nextCard("AT67438T", 1),
	$T = nextCard("AT67438T", 8),
	true = ("AT98765432" =:= "AT98765432"),
	false = ("AT98765432" =:= shuffle("AT98765432")),
	52 = length(oneDeck()),
	104 = length(twoDeck()),
	312 = length(sixDeck()),
	{ok}.
