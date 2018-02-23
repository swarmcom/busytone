-module(t_recipe_voicemail).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check voicemail recipe works after one second"),

	Recipe = ts_make:recipe_with_entry(#{
		conditions => [ #{ name => ticks, args => ['=', 1] }],
		actions => [ #{ name => voicemail, args => [] }]
	}),
	ts_make:dial_in(#{ queue => #{ recipe_id => Recipe } }),

	UUID = ts_make:call(whatever),
	admin:call(subscribe, [uuid, UUID]),

	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_call">> }]  = admin:call(inqueues, []) end),

	ok = call:detect_tone(UUID, "500"),
	call:wait_event(UUID, <<"DETECTED_TONE">>),
	timer:sleep(2000), % voicemail body
	ok = call:hangup(UUID),
	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_vm">> }]  = admin:call(inqueues, []) end),

	timer:sleep(1000), % wait vm to become availablel

	% receive it
	Agent = ts_make:available(),
	[LegA] = agent:wait_for_call(Agent),
	ok = call:answer(LegA),
	call:wait_event(LegA, <<"CHANNEL_ANSWER">>),
	call:wait_hangup(LegA),
	wait(fun() -> []  = admin:call(inqueues, []) end).
