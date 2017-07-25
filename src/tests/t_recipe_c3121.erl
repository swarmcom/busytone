-module(t_recipe_c3121).
-export([main/0]).

% C3121: Tick interval 5 sec and Agents Available is 0 -> Remove German skills, Run many

main() ->
	Queue = admin:new_queue(#{
		skills => #{ english => true, german => true },
		recipe => [ #{
			conditions => [ [ticks, 5] ],
			operations => [ [remove_skills, #{ english => true }] ],
			runs => run_many,
			comment => <<"test">>
		}]
	}),

	Agent = test_lib:available(admin:new_agent( #{ skills => #{ german => true } })),
	{ok, InQueueCall} = call_sup:originate(Queue),
	{'EXIT',{timeout, _}} = (catch agent:wait_for_call(Agent)),

	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(InQueueCall, UUID).
