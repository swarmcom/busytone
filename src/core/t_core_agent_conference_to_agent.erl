-module(t_core_agent_conference_to_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can setup a conference call with another agent"),
	A = test_lib:available(),
	{LegA, LegB} = ts_core:setup_talk(A),
	agent:wait_ev(LegB, LegB, <<"CHANNEL_BRIDGE">>),

	B = test_lib:available(),
	agent:rpc_call(A, conference_to_agent, [skip, B]),
	[LegC] = agent:wait_for_call(B),
	ok = call:answer(LegC),

	agent:wait_ws(A, #{ <<"event">> => <<"agent_state">> }),
	agent:rpc_call(A, inqueue_to_conference, []),

	test_lib:ensureTalking(LegA, LegB),
	test_lib:ensureTalking(LegA, LegC),
	test_lib:ensureTalking(LegB, LegC).