-module(ts_make).
-export([
	dial_in/0, dial_in/1,
	agent/0, agent/1, available/0, available/1, release/1,
	call/1, call_bridged/2,
	recipe_with_entry/1
]).

% create things we're going to test, maybe in different states

dial_in() ->
	ClientId = admin:create(client),
	QueueId = admin:create(queue),
	LineInId = admin:create(line_in, #{ client_id => ClientId, queue_id => QueueId }),
	admin:create(dial, #{ match => <<".*">>, line_in_id => LineInId, header => <<"Caller-Destination-Number">> }).

dial_in(M) ->
	ClientId = admin:create(client, maps:get(client, M, #{})),
	QueueId = admin:create(queue, maps:get(queue, M, #{})),
	LineInId = admin:create(line_in, (maps:get(line_in, M, #{}))#{ client_id => ClientId, queue_id => QueueId }),
	admin:create(dial, #{ match => <<".*">>, line_in_id => LineInId, header => <<"Caller-Destination-Number">> }).

recipe_with_entry(M) ->
	Recipe = admin:create(recipe),
	admin:create(recipe_entry, M#{ recipe_id => Recipe }),
	Recipe.

agent() -> admin:create(agent).
agent(M) -> admin:create(agent, M).

available() -> available(agent()).

available(M) when is_map(M) ->
	available(agent(M));

available(AgentId) ->
	agent:available(AgentId),
	agent:wait_ws(AgentId, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"state">> => <<"available">> } }),
	AgentId.

release(AgentId) ->
	agent:release(AgentId),
	agent:wait_ws(AgentId, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"state">> => <<"release">> } }),
	AgentId.

call(Target) ->
	UUID = call_sup:originate(Target),
	call:wait(UUID).

call_bridged(AgentId, Target) ->
	LegIn = call(Target),
	[LegAgent] = agent:wait_for_call(AgentId),
	ok = call:answer(LegAgent),
	agent:wait_ev(AgentId, LegAgent, <<"CHANNEL_BRIDGE">>),
	{LegIn, LegAgent}.
