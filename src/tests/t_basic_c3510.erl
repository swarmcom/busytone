-module(t_basic_c3510).
-export([main/0]).

% C3510: see: https://ezuce.testrail.com/index.php?/cases/view/3510

main() ->
	Agent1 = test_lib:available(admin:new_agent()),
	{ok, InQueueCall} = call_sup:originate(<<"default_queue">>),
	UUID = test_lib:answer(Agent1),
	test_lib:ensureTalking(InQueueCall, UUID),

	Agent2 = test_lib:available(admin:new_agent()),
	#{<<"transfer_agents">> := #{ Agent2 := #{ <<"state">> := <<"available">> }}} =
		agent:rpc_call(Agent1, <<"ouc.get_transfer_agents">>, []),
	agent:rpc(Agent1, <<"transfer_to_agent">>, [<<"ch1">>, Agent2]),
	agent:wait_ws(Agent1, #{ <<"channelid">> => <<"ch1">>,<<"command">> => <<"endchannel">> }),

	UUID2 = test_lib:answer(Agent2),
	test_lib:ensureTalking(InQueueCall, UUID2).
