-module(t_core_agent_no_answer_seq).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("call is dispatched to another agent on call timeout"),

	ts_make:dial_in(),

	AgentAId = ts_make:available(#{ ring_timeout => 1, max_ring_fails => 1 }),
	AgentBId = ts_make:available(#{ ring_timeout => 1, max_ring_fails => 1 }),

	% sync
	ts_core:wait_agent_state(AgentAId, <<"available">>),

	InLeg = ts_make:call(whatever),
	ts_core:wait_agent_state(AgentAId, <<"release">>),

	[AgentBLeg] = agent:wait_for_call(AgentBId),

	ok = call:answer(AgentBLeg),

	call:hangup(AgentBLeg),
	call:hangup(InLeg).
