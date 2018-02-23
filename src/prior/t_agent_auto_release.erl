-module(t_agent_auto_release).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent is released on hangup"),
	ts_make:dial_in(),
	Agent = ts_make:available(#{ max_ring_fails => 0 }),

	ts_core:wait_agent_state(Agent, <<"available">>),
	ts_make:call(whatever),
	[LegA] = agent:wait_for_call(Agent),
	call:hangup(LegA),
	ts_core:wait_agent_state(Agent, <<"release">>).
