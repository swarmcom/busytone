-module(t_agent_wrapup).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent goes to wrapup state if enabled by queue setting"),
	ts_make:dial_in(#{ queue => #{ wrapup_enabled => true }}),

	Agent = ts_make:available(),
	{LegIn, LegAgent} = ts_make:call_bridged(Agent, whatever),

	call:hangup(LegIn),
	call:wait_hangup(LegAgent),

	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"state">> => <<"wrapup">> } }),
	agent:call(Agent, end_wrapup, []),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"state">> => <<"available">> } }).
