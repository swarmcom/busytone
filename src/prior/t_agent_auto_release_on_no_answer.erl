-module(t_agent_auto_release_on_no_answer).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("available agent goes to release state after no-answer"),
	ts_make:dial_in(),
	AgentId = ts_make:available(#{ ring_timeout => 1, max_ring_fails => 0 }),

	ts_core:wait_agent_state(AgentId, <<"available">>),

	ts_make:call(whatever),
	ts_core:wait_agent_state(AgentId, <<"release">>).
