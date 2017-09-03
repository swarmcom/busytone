-module(t_core_agent_transfer_to_uri).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to external sip uri"),
	Agent = test_lib:available(),
	{LegIn, LegAgent} = ts_core:setup_talk(Agent),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),

	agent:rpc_call(Agent, transfer_to_uri, [<<"external_number">>]),
	#{ <<"Unique-ID">> := LegExt, <<"Caller-Destination-Number">> := <<"external_number">> } = call_sup:wait_call(),
	ok = call:answer(LegExt),

	% agent's leg is hungup, agent is back to available
	wait(fun() -> [ #{ <<"agent_id">> := Agent } ] = admin:agents_queue() end),

	test_lib:ensureTalking(LegIn, LegExt),
	test_lib:ensureTalking(LegExt, LegIn),

	call:hangup(LegIn),
	call:wait_hangup(LegExt).
