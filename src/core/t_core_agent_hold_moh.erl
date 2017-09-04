-module(t_core_agent_hold_moh).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can hold a call and then bridge back"),
	[_Id, Queue] = admin:new_queue(#{
		hold_music => "tone_stream://%(500,0,1600);loops=-1"
	}),
	A = test_lib:available(),
	{LegIn, LegB} = ts_core:setup_talk(A, Queue),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),
	agent:rpc_call(A, hold, []),

	agent:wait_ev(A, LegB, <<"CHANNEL_UNBRIDGE">>),
	agent:wait_ev(A, LegIn, <<"CHANNEL_PARK">>),
	agent:wait_ev(A, LegB, <<"CHANNEL_PARK">>),

	ok = call:detect_tone(LegIn, "1600"),
	call:wait_event(LegIn, <<"DETECTED_TONE">>),

	agent:rpc_call(A, unhold, []),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),

	test_lib:ensureTalking(LegIn, LegB),
	call:hangup(LegIn),
	call:hangup(LegB),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
