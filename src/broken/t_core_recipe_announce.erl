-module(t_core_recipe_announce).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check announce call recipe"),
	[Id, Queue] = admin:new_queue(#{
		skills => #{ german => true },
		recipe => [ #{
			conditions => [ [ticks, '=', 1] ],
			operations => [ [announce, <<"tone_stream://%(500,0,2600)">>] ],
			comment => <<"test">>
		}]
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),
	UUID = test_lib:originate(Queue),
	admin:call(subscribe, [uuid, UUID]),
	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_call">> }]  = admin:call(inqueues, []) end),

	ok = call:detect_tone(UUID, "2600"),
	call:wait_event(UUID, <<"DETECTED_TONE">>),
	ok = call:hangup(UUID).