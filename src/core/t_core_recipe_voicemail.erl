-module(t_core_recipe_voicemail).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check a recipe "),
	[_Id, Queue] = admin:new_queue(#{
		skills => #{ german => true },
		recipe => [ #{
			conditions => [ [ticks, '=', 1] ],
			operations => [ [voicemail ] ],
			comment => <<"test">>
		}]
	}),
	UUID = call_sup:originate(Queue),
	admin:call(subscribe, [uuid, UUID]),
	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_call">> }]  = admin:call(inqueues, []) end),

	ok = call:detect_tone(UUID, "500"),
	call:wait_event(UUID, <<"DETECTED_TONE">>),
	ok = call:hangup(UUID).