-module(ts_make).
-export([dial_in/0, agent/0, call/1, call_bridged/2]).

% umbrella module to setup tests conditions

dial_in() ->
	ClientId = admin:create(client),
	QueueId = admin:create(queue),
	LineInId = admin:create(line_in, #{ client_id => ClientId, queue_id => QueueId }),
	admin:create(dial, #{ match => <<".*">>, line_in_id => LineInId, header => <<"Caller-Destination-Number">> }).

agent() ->
	AgentId = admin:create(agent),
	ts_core:available(AgentId).

call(Target) ->
	UUID = call_sup:originate(Target),
	call:wait(UUID).

call_bridged(AgentId, Target) ->
	LegIn = call(Target),
	[LegAgent] = agent:wait_for_call(AgentId),
	ok = call:answer(LegAgent),
	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_BRIDGE">>),
	{LegIn, LegAgent}.
