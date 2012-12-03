-module(strategy).
-export([act/2, basicStrategies/0, test/0]).


act(Strategies, Context) ->
	{strategy, RegExp, Action} = hd(Strategies),
	case re:run(Context, RegExp) of
		{match, _} -> hd(Action);
		nomatch -> act(tl(Strategies), Context)
	end.



basicStrategies() ->
	[{strategy, "2[01]....", "S"},
		{strategy, "19....", "S"},
		{strategy, "1[87]H...", "S"},
		{strategy, "16H[AT987]..", "H"},
		{strategy, "16H[65432]..", "S"},
		{strategy, "16H...", "P"},
		{strategy, "1[53]H[AT987]..", "H"},
		{strategy, "1[53]H[65432]..", "S"},
		{strategy, "14H[AT987]..", "H"},
		{strategy, "14H[65432]..", "S"},
		{strategy, "14H[765432]..", "P"},
		{strategy, "14H[AT98]..", "H"},
		{strategy, "12H[AT98732]..", "H"},
		{strategy, "12H[654]..", "S"},
		{strategy, "12H[65432]..", "P"},
		{strategy, "12H[AT987]..", "H"},
		{strategy, "11H.N.", "H"},
		{strategy, "11H[T98765432]Y.", "D"},
		{strategy, "11HAY.", "H"},
		{strategy, "10H[98765432]Y.", "D"},
		{strategy, "10H[AT]Y.", "H"},
		{strategy, "10H.N.", "H"},
		{strategy, "09H[6543]Y.", "D"},
		{strategy, "09H[AT9872]Y.", "H"},
		{strategy, "09H.N.", "H"},
		{strategy, "0[875]H...", "H"},
		{strategy, "0[64]H[6543]..", "P"},
		{strategy, "0[64]H[AT9872]..", "H"},
		{strategy, "0[64]H...", "H"},
		{strategy, "18S[65432]Y.", "D"},
		{strategy, "18S[AT9]Y.", "H"},
		{strategy, "18S[87]Y.", "S"},
		{strategy, "18S[AT9]N.", "H"},
		{strategy, "18S[8765432]N.", "S"},
		{strategy, "17S[65432]Y.", "D"},
		{strategy, "17S[AT987]Y.", "H"},
		{strategy, "1[76543]S.N.", "H"},
		{strategy, "1[65]S[6543]Y.", "D"},
		{strategy, "1[65]S[AT9872]Y.", "H"},
		{strategy, "1[43]S[654]Y.", "D"},
		{strategy, "1[43]S[AT98732]Y.", "H"},
		{strategy, "12S...", "P"},
		{strategy, "12S...", "H"}].



test() ->
	Strategies = basicStrategies(),
	"S" = act(Strategies, "21S7YN"),
	"P" = act(Strategies, "06H5YY"),
	"H" = act(Strategies, "12H3NN"),
	{ok}.
