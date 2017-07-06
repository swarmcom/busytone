-module(t_simple).
-export([main/0]).

main() ->
	Agent = agent_sup:agent("agent1", "1234", "agent1"),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	{ok, InQueueCall} = call_sup:originate("sofia/gateway/reach/9999"),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaload">>, <<"channelid">> => <<"ch1">> }),
	call_sup:ensureTalking(InQueueCall, UUID).