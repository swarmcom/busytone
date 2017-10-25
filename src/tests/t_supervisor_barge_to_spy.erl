-module(t_supervisor_barge_to_spy).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and can change spy modes"),
	ts_make:dial_in(),
	Agent = ts_make:available(),
	{InqueueLeg, AgentLeg} = ts_make:call_bridged(Agent, whatever),
	Supervisor = ts_make:agent(),
	agent:call(Supervisor, ws_supervisor, barge, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),

	ts_core:wait_agent_state(Supervisor, <<"barge">>),

	agent:call(Supervisor, ws_supervisor, set_barge_mode, [spy]),
	{'EXIT',{timeout,_}} = (catch test_lib:ensureTalking(SupervisorLeg, AgentLeg, 1000)),
	{'EXIT',{timeout,_}} = (catch test_lib:ensureTalking(SupervisorLeg, InqueueLeg, 1000)),
	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
