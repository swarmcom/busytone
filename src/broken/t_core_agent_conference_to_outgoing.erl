-module(t_core_agent_conference_to_outgoing).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can setup a conference call with a queue"),
	LineId = admin:new_line(#{ number => <<"caller_number">> }),
	A = test_lib:available(admin:new_agent(#{ line_id => LineId })),
	{LegA, LegB} = ts_core:setup_talk(A),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),

	agent:rpc_call(A, conference_to_uri, [<<"external_number">>]),
	#{ <<"Unique-ID">> := LegC, <<"Caller-Destination-Number">> := <<"external_number">> } = call_sup:wait_call(),
	ok = call:answer(LegC),

	agent:wait_ws(A, #{ <<"event">> => <<"agent_state">> }),
	agent:rpc_call(A, inqueue_to_conference, []),

	ts_core:ensure_talking(LegA, LegB),
	ts_core:ensure_talking(LegA, LegC),
	ts_core:ensure_talking(LegB, LegC),

	call:hangup(LegB),
	call:hangup(LegA),
	call:hangup(LegC),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
