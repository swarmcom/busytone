-module(at_dispatch).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	setup(),

	AgentId = ts_make:available(),

	_ExtUUID = ts_make:call("1234"),

	[AgentUUID] = agent:wait_for_call(AgentId),
	ok = call:answer(AgentUUID),
	agent:wait_ev(AgentId, AgentUUID, <<"CHANNEL_BRIDGE">>),
	[#{ <<"agent_id">> := AgentId, <<"uuid">> := AgentUUID, <<"inqueue">> := Inqueue }] = wait(fun() -> [_X] = admin:call(agents, [oncall]) end),
	#{ <<"uuid">> := InqUUID } = Inqueue,
	call:hangup(AgentUUID),
	call:hangup(InqUUID).

setup() ->
	ts_make:dial_in().
