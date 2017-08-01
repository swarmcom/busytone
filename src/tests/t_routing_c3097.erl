-module(t_routing_c3097).
-export([main/0]).

% C3097: Call priority and wrapup settings when transferred from one queue to another with same skills
% https://ezuce.testrail.com/index.php?/cases/view/3097

main() ->
	Queue1 = admin:new_queue(#{ wrapup_enabled => false, skills => #{ '_queue' => true }, recipe => [] }),
	Queue2 = admin:new_queue(#{ wrapup_enabled => true, skills => #{ '_queue' => true }, recipe => [] }),

	Agent1 = test_lib:available(admin:new_agent(#{ ring_timeout => 25, skills => #{ '_queue' => Queue1 } })),
	Agent2 = test_lib:available(admin:new_agent(#{ ring_timeout => 25, skills => #{ '_queue' => Queue2 } })),

	{ok, InCall1} = call_sup:originate(Queue1),
	UUID1 = test_lib:answer(Agent1),
	test_lib:ensureTalking(UUID1, InCall1),

	{ok, InCall2} = call_sup:originate(Queue2),
	UUID2 = test_lib:answer(Agent2),
	test_lib:ensureTalking(UUID2, InCall2),

	{ok, InCall3} = call_sup:originate(Queue2),

	agent:rpc_call(Agent1, <<"transfer_to_queue">>, [<<"ch1">>, Queue2, #{ skills => [] }]),
	call:wait_hangup(UUID1),

	#{<<"channel">> := <<"ch1">>,<<"state">> := <<"wrapup">>} = agent:rpc_call(Agent2, <<"hangup">>, [<<"ch1">>]),
	agent:rpc_call(Agent2, <<"end_wrapup">>, [<<"ch1">>]),
	agent:wait_ws(Agent2, #{ <<"command">> => <<"endchannel">> }),

	UUID2_1 = test_lib:answer(Agent2, <<"ch2">>),
	test_lib:ensureTalking(UUID2_1, InCall1),

	agent:rpc_call(Agent2, <<"transfer_to_queue">>, [<<"ch2">>, Queue1, #{ skills => [] }]),
	agent:rpc_call(Agent2, <<"end_wrapup">>, [<<"ch2">>]),

	UUID2_2 = test_lib:answer(Agent2, <<"ch3">>),
	test_lib:ensureTalking(UUID2_2, InCall3),
	agent:rpc_call(Agent2, <<"hangup">>, [<<"ch3">>]),

	UUID1_1 = test_lib:answer(Agent1, <<"ch2">>),
	test_lib:ensureTalking(UUID1_1, InCall1),
	agent:rpc_call(Agent1, <<"hangup">>, [<<"ch2">>]),
	agent:wait_ws(Agent1, #{ <<"command">> => <<"endchannel">> }).
