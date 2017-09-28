-module(t_core_supervisor_barge).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can barge in existing call, and talk to each party"),
	Agent = test_lib:available(),
	{InqueueLeg, AgentLeg} = ts_core:setup_talk(Agent),
	Supervisor = admin:new_agent(),
	agent:rpc_call(Supervisor, ws_supervisor, barge, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),
	test_lib:ensureTalking(InqueueLeg, SupervisorLeg),
	test_lib:ensureTalking(SupervisorLeg, InqueueLeg),
	test_lib:ensureTalking(AgentLeg, SupervisorLeg),
	test_lib:ensureTalking(SupervisorLeg, AgentLeg),
	test_lib:ensureTalking(AgentLeg, InqueueLeg),
	test_lib:ensureTalking(InqueueLeg, AgentLeg),
	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
