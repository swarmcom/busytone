-module(t_core_agent_transfer_to_outgoing).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to external sip uri"),
	LineId = admin:new_line(#{ number => <<"caller_number">> }),
	Agent = test_lib:available(admin:new_agent(#{ line_id => LineId })),
	{LegIn, LegAgent} = ts_core:setup_talk(Agent),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),

	agent:rpc_call(Agent, transfer_to_uri, [<<"external_number">>]),
	#{ <<"Unique-ID">> := LegExt, <<"Caller-Destination-Number">> := <<"external_number">> } = call_sup:wait_call(),
	ok = call:answer(LegExt),

	% agent's leg is hungup, agent is back to available
	wait(fun() -> [ #{ <<"agent_id">> := Agent } ] = admin:agents_queue() end),

	ts_core:ensure_talking(LegIn, LegExt),
	ts_core:ensure_talking(LegExt, LegIn),

	call:hangup(LegIn),
	call:wait_hangup(LegExt).
