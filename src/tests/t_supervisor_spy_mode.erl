-module(t_supervisor_spy_mode).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and can change spy modes"),

	ts_make:dial_in(),
	Agent = ts_make:available(),
	{InqueueLeg, AgentLeg} = ts_make:call_bridged(Agent, whatever),
	Supervisor = ts_make:agent(),
	agent:call(Supervisor, ws_supervisor, spy, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),

	{'EXIT',{timeout,_}} = (catch ts_core:ensure_talking(SupervisorLeg, AgentLeg, 1000)),
	agent:call(Supervisor, ws_supervisor, set_barge_mode, [agent]),
	ts_core:ensure_talking(SupervisorLeg, AgentLeg),

	{'EXIT',{timeout,_}} = (catch ts_core:ensure_talking(SupervisorLeg, InqueueLeg, 1000)),
	agent:call(Supervisor, ws_supervisor, set_barge_mode, [caller]),
	ts_core:ensure_talking(SupervisorLeg, InqueueLeg),

	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
