-module(t_routing_c3539).
-export([main/0]).

% C3539: Magic skill - queue routing

main() ->
	Queue = admin:new_queue(#{
		skills => #{ '_queue' => true },
		recipe => []
	}),

	_Agent1 = test_lib:available(admin:new_agent()),
	timer:sleep(1000),
	Agent2 = test_lib:available(admin:new_agent(#{ skills => #{ '_queue' => Queue }})),

	{ok, InQueueCall} = call_sup:originate(Queue),
	test_lib:ensureTalking(InQueueCall, test_lib:answer(Agent2)).