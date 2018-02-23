-module(t_supervisor_barge).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can barge in existing call, and talk to each party"),
	ts_make:dial_in(),
	Agent = ts_make:available(),
	{InqueueLeg, AgentLeg} = ts_make:call_bridged(Agent, whatever),
	Supervisor = ts_make:agent(),
	agent:call(Supervisor, ws_supervisor, barge, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),

	ts_core:wait_agent_state(Supervisor, <<"barge">>),

	ts_core:ensure_talking(InqueueLeg, SupervisorLeg),
	ts_core:ensure_talking(SupervisorLeg, InqueueLeg),
	ts_core:ensure_talking(AgentLeg, SupervisorLeg),
	ts_core:ensure_talking(SupervisorLeg, AgentLeg),
	ts_core:ensure_talking(AgentLeg, InqueueLeg),
	ts_core:ensure_talking(InqueueLeg, AgentLeg),
	
	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
