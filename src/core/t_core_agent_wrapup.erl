-module(t_core_agent_wrapup).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent goes to wrapup state if enabled by queue"),
	[Id, Queue] = admin:new_queue(#{
		wrapup_enabled => true
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),
	Agent = test_lib:available(),
	{LegIn, LegB} = ts_core:setup_talk(Agent, Queue),
	call:hangup(LegIn),
	call:wait_hangup(LegB),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"wrapup">> } }),
	agent:rpc_call(Agent, end_wrapup, []),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"available">> } }).