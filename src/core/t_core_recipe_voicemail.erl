-module(t_core_recipe_voicemail).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check voicemail recipe works after one second"),
	[Id, Queue] = admin:new_queue(#{
		recipe => [ #{
			conditions => [ [ticks, '=', 1] ],
			operations => [ [voicemail ] ]
		}]
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),
	UUID = test_lib:originate(Queue),
	admin:call(subscribe, [uuid, UUID]),
	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_call">> }]  = admin:call(inqueues, []) end),

	ok = call:detect_tone(UUID, "500"),
	call:wait_event(UUID, <<"DETECTED_TONE">>),
	timer:sleep(2000), % voicemail body
	ok = call:hangup(UUID),
	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_vm">> }]  = admin:call(inqueues, []) end),

	% receive it
	Agent = test_lib:available(),
	[LegA] = agent:wait_for_call(Agent),
	ok = call:answer(LegA),
	call:wait_event(LegA, <<"CHANNEL_ANSWER">>),
	call:wait_hangup(LegA),
	wait(fun() -> []  = admin:call(inqueues, []) end).
