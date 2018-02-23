-module(t_agent_conference_to_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can setup a conference call with another agent"),

	ts_make:dial_in(),
	Agent = ts_make:available(),

	{LegIn, LegAgent} = ts_make:call_bridged(Agent, whatever),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),

	AgentB = ts_make:available(),
	agent:call(Agent, conference_to_agent, [AgentB]),

	[LegAgentB] = agent:wait_for_call(AgentB),
	ok = call:answer(LegAgentB),

	ts_core:wait_agent_state(Agent, <<"conference">>),

	agent:call(Agent, conference, [inqueue]),

	ts_core:ensure_talking(LegIn, LegAgent),
	ts_core:ensure_talking(LegIn, LegAgentB),
	ts_core:ensure_talking(LegAgent, LegAgentB),

	call:hangup(LegAgent),
	call:hangup(LegIn),
	call:hangup(LegAgentB),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
