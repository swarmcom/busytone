-module(t_agent_transfer_to_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to another agent"),

	ts_make:dial_in(),

	Agent = ts_make:available(),
	ts_make:call_bridged(Agent, whatever),

	AgentB = ts_make:available(),

	agent:call(Agent, transfer_to_agent, [AgentB]),
	[UUID] = agent:wait_for_call(AgentB),
	call:answer(UUID),
	agent:wait_ev(AgentB, UUID, <<"CHANNEL_BRIDGE">>),
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"agent_id">> := Agent }, #{ <<"agent_id">> := AgentB } ] = admin:agents_queue() end).
