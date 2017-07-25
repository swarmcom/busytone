-module(t_recipe_c3118).
-export([main/0]).

% C3118: Media is Voice -> Add skills English, Run many

main() ->
	Queue = admin:new_queue(#{
		recipe => [ #{
			conditions => [ [type, '=', voice] ],
			operations => [ [add_skills, #{ english => true }] ],
			runs => run_many,
			comment => <<"test">>
		}]
	}),

	Agent = test_lib:available(admin:new_agent( #{ skills => #{ english => true } })),
	{ok, InQueueCall} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(InQueueCall, UUID),

	Agent1 = test_lib:available(admin:new_agent( #{ skills => #{ } })),
	{ok, _} = call_sup:originate(Queue),
	{'EXIT',{timeout, _}} = (catch agent:wait_for_call(Agent1)).
