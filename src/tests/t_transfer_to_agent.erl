-module(t_transfer_to_agent).
-export([main/0]).

main() ->
	Agent1 = test_lib:login_available("agent1", "1234", "agent1"),
	{ok, InQueueCall} = call_sup:originate("sofia/gateway/reach/9999"),
	agent:wait_ws(Agent1, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),
	[UUID] = agent:wait_for_call(Agent1),
	ok = call:answer(UUID),
	agent:wait_ws(Agent1, #{ <<"command">> => <<"mediaload">>, <<"channelid">> => <<"ch1">> }),
	test_lib:ensureTalking(InQueueCall, UUID),

	_Agent2 = test_lib:login_available("agent2", "1234", "agent2").
