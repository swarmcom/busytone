-module(t_core_agent_hold_moh).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can hold a call and then bridge back"),
	[Id, Queue] = admin:new_queue(#{
		hold_music => "tone_stream://%(500,0,1600);loops=-1"
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),
	A = ts_make:available(),
	{LegIn, LegB} = ts_core:setup_talk(A, Queue),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),
	agent:call(A, hold, []),

	agent:wait_ev(A, LegB, <<"CHANNEL_UNBRIDGE">>),
	agent:wait_ev(A, LegIn, <<"CHANNEL_PARK">>),
	agent:wait_ev(A, LegB, <<"CHANNEL_PARK">>),

	ok = call:detect_tone(LegIn, "1600"),
	call:wait_event(LegIn, <<"DETECTED_TONE">>),

	agent:call(A, unhold, []),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),

	ts_core:ensure_talking(LegIn, LegB),
	call:hangup(LegIn),
	call:hangup(LegB),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
