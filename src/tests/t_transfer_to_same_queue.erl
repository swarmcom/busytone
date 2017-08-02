-module(t_transfer_to_same_queue).
-export([main/0]).

% test an agent recieve a call back just after transferring call to the same queue

main() ->
	Queue = admin:new_queue(#{
		skills => #{ english => true },
		recipe => []
	}),

	Agent = test_lib:available(admin:new_agent(#{ skills => #{ english => true }})),
	{ok, InQueueCall} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(UUID, InQueueCall),

	agent:rpc_call(Agent, <<"transfer_to_queue">>, [<<"ch1">>, Queue, #{ skills => [ <<"english">> ]}]),

	Agent1 = test_lib:available(admin:new_agent(#{ skills => #{} })),
	{'EXIT', {timeout, _}} = (catch agent:wait_for_call(Agent1)),

	agent:rpc_call(Agent, <<"end_wrapup">>, [<<"ch1">>]),

	UUID2 = test_lib:answer(Agent, <<"ch2">>),
	test_lib:ensureTalking(UUID2, InQueueCall).
