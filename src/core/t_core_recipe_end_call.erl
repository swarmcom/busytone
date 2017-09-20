-module(t_core_recipe_end_call).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check end call recipe action"),
	[Id, Queue] = admin:new_queue(#{
		skills => #{ german => true },
		recipe => [ #{
			conditions => [ [ticks, '=', 1] ],
			operations => [ [end_call] ]
		}]
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),
	UUID = test_lib:originate(Queue),
	admin:call(subscribe, [uuid, UUID]),
	wait(fun() -> []  = admin:call(inqueues, []) end).
