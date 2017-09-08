-module(t_core_queue_moh).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check incoming call has proper moh settings"),
	[_Id, Queue] = admin:new_queue(#{
		hold_music => "tone_stream://%(500,0,1600);loops=-1"
	}),
	UUID = test_lib:originate(Queue),

	ok = call:detect_tone(UUID, "1600"),
	call:wait_event(UUID, <<"DETECTED_TONE">>),
	ok = call:hangup(UUID).
