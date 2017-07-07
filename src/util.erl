-module(util).
-export([match_maps/2]).

match_maps(_, undefined) -> false;
match_maps(Inner, Outer) ->
	L = maps:to_list(Inner) -- maps:to_list(Outer),
	case erlang:length(L) of
		0 -> true;
		_N -> false
	end.
