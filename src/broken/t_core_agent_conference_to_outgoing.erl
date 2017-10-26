-module(t_core_agent_conference_to_outgoing).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can setup a conference call with a queue"),

	ts_make:dial_in(),

	LineOutId = admin:create(line_out, #{
		caller_id_name => <<"caller_name">>,
		caller_id_number => <<"caller_number">>,
		override_clid => true
	}),
	Agent = ts_make:available(#{ line_id => LineOutId }),

	{LegIn, LegAgent} = ts_make:call_bridged(Agent, whatever),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),

	agent:call(Agent, conference_to_uri, [<<"external_number">>]),
	#{ <<"Unique-ID">> := LegC, <<"Caller-Destination-Number">> := <<"external_number">> } = call_sup:wait_call(),
	ok = call:answer(LegC),

	agent:call(Agent, conference, [inqueue]),

	ts_core:ensure_talking(LegIn, LegAgent),
	ts_core:ensure_talking(LegIn, LegC),
	ts_core:ensure_talking(LegAgent, LegC),

	call:hangup(LegAgent),
	call:hangup(LegIn),
	call:hangup(LegC),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
