-module(t_agent_transfer_to_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to a queue"),

	DialInId = ts_make:dial_in(),
	QueueId = ts_core:path([<<"line_in">>, <<"queue_id">>], admin:get(dial, DialInId)),

	Agent = ts_make:available(),
	ts_make:call_bridged(Agent, whatever),

	AgentB = ts_make:available(),

	agent:call(Agent, transfer_to_queue, [QueueId]),

	[UUID] = agent:wait_for_call(AgentB), % it should be B-agent to get a call, as it has longest wait time in available state
	call:answer(UUID),
	agent:wait_ev(AgentB, UUID, <<"CHANNEL_BRIDGE">>),
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"agent_id">> := Agent }, #{ <<"agent_id">> := AgentB } ] = admin:agents_queue() end).
