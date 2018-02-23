-module(t_recipe_hangup).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check recipe hangup works"),

	Recipe = ts_make:recipe_with_entry(#{
		conditions => [ #{ name => ticks, args => ['=', 1] }],
		actions => [ #{ name => hangup, args => [] }]
	}),
	ts_make:dial_in(#{ queue => #{ recipe_id => Recipe } }),

	UUID = ts_make:call(whatever),
	admin:call(subscribe, [uuid, UUID]),
	wait(fun() -> []  = admin:call(inqueues, []) end).
