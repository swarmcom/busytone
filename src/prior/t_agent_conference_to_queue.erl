-module(t_agent_conference_to_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can setup a conference call with a queue"),

	DialInId = ts_make:dial_in(),
	QueueId = ts_core:path([<<"line_in">>, <<"queue_id">>], admin:get(dial, DialInId)),

	Agent = ts_make:available(),
	{LegIn, LegAgent} = ts_make:call_bridged(Agent, whatever),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),

	agent:call(Agent, conference_to_queue, [QueueId]),

	AgentB = ts_make:available(),
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
