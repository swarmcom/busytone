-module(t_supervisor_spy_to_barge).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and then switch to barge mode"),

	ts_make:dial_in(),
	Agent = ts_make:available(),
	{InqueueLeg, AgentLeg} = ts_make:call_bridged(Agent, whatever),
	Supervisor = ts_make:agent(),
	agent:call(Supervisor, ws_supervisor, spy, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),

	ts_core:wait_agent_state(Supervisor, <<"barge">>),

	agent:call(Supervisor, ws_supervisor, set_barge_mode, [barge]),
	ts_core:ensure_talking(SupervisorLeg, AgentLeg),
	ts_core:ensure_talking(SupervisorLeg, InqueueLeg),

	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
