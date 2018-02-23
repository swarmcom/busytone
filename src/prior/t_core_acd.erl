-module(t_core_acd).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("automatic call distribution"),
	ts_make:dial_in(),
	AgentA = ts_make:available(#{ max_ring_fails => 3 }),
	AgentB = ts_make:available(#{ max_ring_fails => 3 }),
	_AgentC = ts_make:available(#{ max_ring_fails => 3 }),

	ts_make:call(whatever),

	[LegA] = agent:wait_for_call(AgentA),
	call:hangup(LegA),

	[LegB] = agent:wait_for_call(AgentB),
	call:hangup(LegB),

	[LegA2] = agent:wait_for_call(AgentA),
	call:hangup(LegA2).
