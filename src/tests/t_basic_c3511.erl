-module(t_basic_c3511).
-export([main/0]).

% C3511: conference with another agent

main() ->
	Queue = admin:new_queue(#{
		skills => #{ english => true },
		recipe => []
	}),

	Agent1 = test_lib:available(admin:new_agent(#{ perm_profile => supervisor, skills => #{ english => true }})),
	Agent2 = test_lib:available(admin:new_agent(#{ skills => #{ german => true }})),

	{ok, InQueueCall} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent1),
	test_lib:ensureTalking(InQueueCall, UUID),

	agent:rpc_call(Agent1, <<"conference_to_agent">>, [<<"ch1">>, Agent2]),

	[UUID2] = agent:wait_for_call(Agent2),
	ok = call:answer(UUID2),
	call:wait_event(UUID2, #{ <<"Event-Name">> => <<"CHANNEL_ANSWER">> }),

	test_lib:ensureTalking(InQueueCall, UUID2),
	test_lib:ensureTalking(UUID, UUID2).
