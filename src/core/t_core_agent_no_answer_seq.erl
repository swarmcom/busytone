-module(t_core_agent_no_answer_seq).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("call is dispatched to another agent on call failure"),
	AgentA = admin:new_agent(#{ ring_timeout => 1, max_ring_fails => 1 }),
	test_lib:available(AgentA),
	AgentB = admin:new_agent(#{ ring_timeout => 1, max_ring_fails => 1 }),
	test_lib:available(AgentB),

	ts_core:wait_agent_state(AgentA, <<"available">>),

	InLeg = test_lib:originate(<<"default_queue">>),
	ts_core:wait_agent_state(AgentA, <<"release">>),

	[AgentBLeg] = agent:wait_for_call(AgentB),
	ok = call:answer(AgentBLeg),
	agent:wait_ev(AgentB, AgentBLeg, <<"CHANNEL_BRIDGE">>),
	call:hangup(AgentBLeg),
	call:hangup(InLeg).
