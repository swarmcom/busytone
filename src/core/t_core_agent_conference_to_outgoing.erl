-module(t_core_agent_conference_to_outgoing).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can setup a conference call with a queue"),
	A = test_lib:available(),
	{LegA, LegB} = ts_core:setup_talk(A),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),

	agent:rpc_call(A, conference_to_uri, [<<"external_number">>]),
	#{ <<"Unique-ID">> := LegC, <<"Caller-Destination-Number">> := <<"external_number">> } = call_sup:wait_call(),
	ok = call:answer(LegC),

	agent:wait_ws(A, #{ <<"event">> => <<"agent_state">> }),
	agent:rpc_call(A, inqueue_to_conference, []),

	test_lib:ensureTalking(LegA, LegB),
	test_lib:ensureTalking(LegA, LegC),
	test_lib:ensureTalking(LegB, LegC),

	call:hangup(LegB),
	call:hangup(LegA),
	call:hangup(LegC),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).