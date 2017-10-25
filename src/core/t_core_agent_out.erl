-module(t_core_agent_out).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check outgoing call works for an agent"),

	LineOutId = admin:create(line_out, #{
		caller_id_name => <<"caller_name">>,
		caller_id_number => <<"caller_number">>,
		override_clid => true
	}),
	AgentId = admin:create(agent, #{ line_id => LineOutId }),

	agent:call(AgentId, ws_agent, call, [some_destination]),
	[LegAgent] = agent:wait_for_call(AgentId),

	ok = call:answer(LegAgent),
	#{
		<<"Unique-ID">> := LegB,
		<<"Caller-Destination-Number">> := <<"some_destination">>,
		<<"Caller-Caller-ID-Number">> := <<"caller_number">>,
		<<"Caller-Caller-ID-Name">> := <<"caller_name">>
	} = call_sup:wait_call(),

	ok = call:answer(LegB),
	call:hangup(LegAgent).

