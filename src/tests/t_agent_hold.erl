-module(t_agent_hold).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can hold a call, and then can talk again"),
	ts_make:dial_in(),
	AgentId = ts_make:agent(),
	{LegIn, LegAgent} = ts_core:setup_talk(AgentId),

	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_BRIDGE">>),
	agent:call(AgentId, ws_agent, hold, []),

	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_UNBRIDGE">>),
	agent:wait_ev(AgentId, LegIn, <<"CHANNEL_PARK">>),
	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_PARK">>),

	agent:call(AgentId, ws_agent, unhold, []),
	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_BRIDGE">>),

	ts_core:ensure_talking(LegIn, LegAgent),
	call:hangup(LegIn),
	call:hangup(LegAgent),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
