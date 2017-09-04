-module(t_core_agent_no_answer).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("available agent goes to release state after no-answer"),
	Agent = admin:new_agent(#{ ring_timeout => 1, max_ring_fails => 1 }),
	test_lib:available(Agent),
	ts_core:wait_agent_state(Agent, <<"available">>),
	call_sup:originate(<<"default_queue">>),
	ts_core:wait_agent_state(Agent, <<"release">>).
