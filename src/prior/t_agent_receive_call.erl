-module(t_agent_receive_call).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can place a call to a queue and receive it"),
	ts_make:dial_in(),
	AgentId = ts_make:available(),
	{LegIn, LegAgent} = ts_make:call_bridged(AgentId, target),
	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_BRIDGE">>),
	call:hangup(LegAgent),
	call:wait_hangup(LegIn).
