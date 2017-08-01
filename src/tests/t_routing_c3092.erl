-module(t_routing_c3092).
-export([main/0]).

% C3092: Routing to the agent who is available for longest time

receive_call(Queue, Agent, Ch) ->
	{ok, InCall} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent, Ch),
	test_lib:ensureTalking(UUID, InCall),
	test_lib:hangup(UUID).

main() ->
	Queue = admin:new_queue(#{ wrapup_enabled => false, recipe => [] }),

	% XXX: ensure all a vqueue is created 
	{ok, InCall} = call_sup:originate(Queue),
	test_lib:hangup(InCall),

	Agent1 = test_lib:available(admin:new_agent(#{ ring_timeout => 6 })),
	timer:sleep(200),
	Agent2 = test_lib:available(admin:new_agent(#{ ring_timeout => 6 })),
	timer:sleep(200),
	Agent3 = test_lib:available(admin:new_agent(#{ ring_timeout => 6 })),

	receive_call(Queue, Agent1, <<"ch1">>),
	receive_call(Queue, Agent2, <<"ch1">>),
	receive_call(Queue, Agent3, <<"ch1">>),
	receive_call(Queue, Agent1, <<"ch2">>),
	receive_call(Queue, Agent2, <<"ch2">>),
	receive_call(Queue, Agent3, <<"ch2">>).
