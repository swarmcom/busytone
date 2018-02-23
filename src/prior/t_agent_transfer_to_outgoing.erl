-module(t_agent_transfer_to_outgoing).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to external sip uri"),

	ts_make:dial_in(),

	LineOutId = admin:create(line_out, #{
		caller_id_name => <<"caller_name">>,
		caller_id_number => <<"caller_number">>,
		override_clid => true
	}),
	Agent = ts_make:available(#{ line_id => LineOutId }),

	{LegIn, LegAgent} = ts_make:call_bridged(Agent, whatever),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),

	agent:call(Agent, transfer_to_uri, [<<"external_number">>]),

	#{ <<"Unique-ID">> := LegExt, <<"Caller-Destination-Number">> := <<"external_number">> } = call_sup:wait_call(),
	ok = call:answer(LegExt),

	% agent's leg is hungup, agent is back to available
	wait(fun() -> [ #{ <<"agent_id">> := Agent } ] = admin:agents_queue() end),

	ts_core:ensure_talking(LegIn, LegExt),
	ts_core:ensure_talking(LegExt, LegIn),

	call:hangup(LegIn),
	call:wait_hangup(LegExt).
