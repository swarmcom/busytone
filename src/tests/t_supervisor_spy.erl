-module(t_supervisor_spy).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and can't talk to each party"),

	ts_make:dial_in(),
	Agent = ts_make:available(),
	{InqueueLeg, AgentLeg} = ts_make:call_bridged(Agent, whatever),
	Supervisor = ts_make:agent(),
	agent:call(Supervisor, ws_supervisor, spy, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),

	ts_core:wait_agent_state(Supervisor, <<"barge">>),

	{'EXIT',{timeout,_}} = (catch ts_core:ensure_talking(SupervisorLeg, AgentLeg, 1000)),
	{'EXIT',{timeout,_}} = (catch ts_core:ensure_talking(SupervisorLeg, InqueueLeg, 1000)),
	ts_core:ensure_talking(InqueueLeg, SupervisorLeg),
	ts_core:ensure_talking(AgentLeg, SupervisorLeg),
	ts_core:ensure_talking(AgentLeg, InqueueLeg),
	ts_core:ensure_talking(InqueueLeg, AgentLeg),
	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
