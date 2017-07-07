-module(t_transfer_to_agent).
-export([main/0]).

% see: https://ezuce.testrail.com/index.php?/cases/view/3510

main() ->
	Agent1 = test_lib:login_available("agent1", "1234", "agent1"),
	{ok, InQueueCall} = call_sup:originate("sofia/gateway/reach/9999"),
	agent:wait_ws(Agent1, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),
	[UUID] = agent:wait_for_call(Agent1),
	ok = call:answer(UUID),
	agent:wait_ws(Agent1, #{ <<"command">> => <<"mediaload">>, <<"channelid">> => <<"ch1">> }),
	test_lib:ensureTalking(InQueueCall, UUID),
	Agent2 = test_lib:login_available("agent2", "1234", "agent2"),
	Id1 = agent:rpc(Agent1, <<"ouc.get_transfer_agents">>, []),
	{match, _Ts, #{ <<"result">> := #{<<"transfer_agents">> := #{ Agent2 := #{ <<"state">> := <<"available">> } }}}}
		= agent:wait_ws(Agent1, #{ <<"id">> => Id1 }),
	agent:rpc(Agent1, <<"transfer_to_agent">>, [<<"ch1">>, Agent2]),
	agent:wait_ws(Agent1, #{ <<"channelid">> => <<"ch1">>,<<"command">> => <<"endchannel">> }),
	[UUID2] = agent:wait_for_call(Agent2),
	ok = call:answer(UUID2),
	test_lib:ensureTalking(InQueueCall, UUID2).
