-module(t_core_agent_record).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can start recording"),
	Agent = test_lib:available(),
	{LegIn, LegAgent} = ts_core:setup_talk(Agent),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),
	agent:rpc_call(Agent, record, [start]),
	timer:sleep(2000),
	call:hangup(LegIn),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"available">> } }),
	agent:rpc_call(Agent, record, [play, LegIn]),
	[Play] = agent:wait_for_call(Agent),
	call:answer(Play),
	call:wait_hangup(Play),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"available">> } }).
