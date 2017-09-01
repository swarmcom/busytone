-module(t_core_agent_hold).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can hold a call and then bridge back"),
	A = test_lib:available(),
	{LegA, LegB} = ts_core:setup_talk(A),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),
	agent:rpc_call(A, hold, []),

	agent:wait_ev(A, LegB, <<"CHANNEL_UNBRIDGE">>),
	agent:wait_ev(A, LegA, <<"CHANNEL_PARK">>),
	agent:wait_ev(A, LegB, <<"CHANNEL_PARK">>),

	agent:rpc_call(A, unhold, []),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),

	test_lib:ensureTalking(LegA, LegB),
	call:hangup(LegA),
	call:hangup(LegB),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
