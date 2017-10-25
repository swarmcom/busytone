-module(t_core_supervisor_spy_barge).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and then switch to barge mode"),
	Agent = test_lib:available(),
	{InqueueLeg, AgentLeg} = ts_core:setup_talk(Agent),
	Supervisor = admin:new_agent(),
	agent:rpc_call(Supervisor, ws_supervisor, spy, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),
	agent:wait_ws(Supervisor, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"barge">> } }),

	agent:rpc_call(Supervisor, ws_supervisor, set_barge_mode, [barge]),
	test_lib:ensureTalking(SupervisorLeg, AgentLeg),
	test_lib:ensureTalking(SupervisorLeg, InqueueLeg),

	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
