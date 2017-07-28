-module(t_routing_c3099).
-export([main/0]).

% C3099: Call priority when transferred from one queue to another with different skills
% https://ezuce.testrail.com/index.php?/cases/view/3099

main() ->
	Queue1 = admin:new_queue(#{ wrapup_enabled => false, recipe => [], skills => #{ english => true } }),
	Queue2 = admin:new_queue(#{ wrapup_enabled => false, recipe => [], skills => #{ german => true } }),

	Agent1 = test_lib:available(admin:new_agent(#{ skills => #{ english => true } })),
	Agent2 = test_lib:available(admin:new_agent(#{ skills => #{ german => true } })),

	{ok, InCall1} = call_sup:originate(Queue1),
	UUID1 = test_lib:answer(Agent1),
	test_lib:ensureTalking(UUID1, InCall1),

	{ok, InCall2} = call_sup:originate(Queue2),
	UUID2 = test_lib:answer(Agent2),
	test_lib:ensureTalking(UUID2, InCall2),

	{ok, InCall3} = call_sup:originate(Queue2),
	agent:rpc_call(Agent1, <<"transfer_to_queue">>, [<<"ch1">>, Queue2, #{ skills => [ <<"german">> ]}]),

	test_lib:hangup(UUID2),

	UUID2_1 = test_lib:answer(Agent2, <<"ch2">>),
	test_lib:ensureTalking(UUID2_1, InCall1),
	test_lib:hangup(UUID2_1),

	UUID2_2 = test_lib:answer(Agent2, <<"ch3">>),
	test_lib:ensureTalking(UUID2_2, InCall3),
	test_lib:hangup(UUID2_2).
