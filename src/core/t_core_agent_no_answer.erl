-module(t_core_agent_no_answer).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("available agent goes to release state after no-answer"),
	ts_make:dial_in(),
	AgentId = admin:create(agent, #{ ring_timeout => 1, max_ring_fails => 1 }),
	ts_core:available(AgentId),
	ts_core:wait_agent_state(AgentId, <<"available">>),

	ts_make:call(whatever),
	ts_core:wait_agent_state(AgentId, <<"release">>).
