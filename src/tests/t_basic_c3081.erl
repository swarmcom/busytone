-module(t_basic_c3081).
-export([main/0]).

% C3081: transfer to external number

main() ->
	Queue = admin:new_queue(#{
		skills => #{},
		recipe => []
	}),

	Agent = test_lib:available(admin:new_agent(#{ perm_profile => supervisor, skills => #{} })),
	{ok, InQueueCall} = call_sup:originate(Queue),

	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(InQueueCall, UUID),

	agent:rpc_call(Agent, <<"transfer_outband">>, [<<"ch1">>, <<"external">>]),
	#{ <<"Caller-Destination-Number">> := <<"external">>, <<"Caller-Logical-Direction">> := <<"inbound">> } = call_sup:wait_call().
