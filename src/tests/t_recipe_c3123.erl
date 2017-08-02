-module(t_recipe_c3123).
-export([main/0]).

% C3123: Agents Eligible is less than 1 -> Send to voicemail, Run many

main() ->
	Queue = admin:new_queue(#{
		wrapup_enabled => false,
		skills => #{ english => true, german => true },
		recipe => [ #{
			conditions => [ [eligible_agents, '<', 1], [type, '=', voice] ],
			operations => [ [voicemail, [] ] ],
			runs => run_many,
			comment => <<"test">>
		}]
	}),

	_Agent1 = test_lib:available(admin:new_agent( #{ skills => #{ english => true } })),
	test_lib:leave_voicemail(Queue),

	Agent = admin:new_agent( #{ skills => #{ english => true, german => true } }),
	{ok, InQueueCall} = call_sup:originate(Queue),
	test_lib:available(Agent),
	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(UUID, InQueueCall),
	agent:rpc_call(Agent, <<"hangup">>, [<<"ch1">>]),
	test_lib:receive_voicemail(Agent).

