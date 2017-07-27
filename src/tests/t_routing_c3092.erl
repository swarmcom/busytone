-module(t_routing_c3092).
-export([main/0]).

% C3092: Routing to the agent who is available for longest time

receive_call(Queue, Agent) ->
	{ok, InCall} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(UUID, InCall),
	call:hangup(UUID).

main() ->
	Queue = admin:new_queue(#{
		recipe => []
	}),

	Agent1 = test_lib:available(admin:new_agent()),
	timer:sleep(1000),
	Agent2 = test_lib:available(admin:new_agent()),
	timer:sleep(1000),
	Agent3 = test_lib:available(admin:new_agent()),

	receive_call(Queue, Agent1),
	receive_call(Queue, Agent2),
	receive_call(Queue, Agent3).
