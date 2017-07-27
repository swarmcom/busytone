-module(t_basic_c3079).
-export([main/0]).

% C3079: Transfer to another queue

main() ->
	Queue1 = admin:new_queue(#{
		skills => #{ english => true },
		recipe => []
	}),

	Queue2 = admin:new_queue(#{
		skills => #{ german => true },
		recipe => []
	}),

	Agent1 = test_lib:available(admin:new_agent(#{ skills => #{ english => true }})),
	{ok, InQueueCall} = call_sup:originate(Queue1),
	UUID1 = test_lib:answer(Agent1),
	test_lib:ensureTalking(UUID1, InQueueCall),

	agent:rpc_call(Agent1, <<"transfer_to_queue">>, [<<"ch1">>, Queue2, #{ skills => [ <<"german">> ]}]),

	Agent2 = test_lib:available(admin:new_agent(#{ skills => #{ german => true }})),
	UUID2 = test_lib:answer(Agent2),
	test_lib:ensureTalking(UUID2, InQueueCall).
