-module(t_recipe_c3116).
-export([main/0]).

% C3116: Route all calls to VM

main() ->
	Queue = admin:new_queue(#{
		recipe => [ #{
			conditions => [ [type, '=', voice] ],
			operations => [ [voicemail, []] ],
			runs => run_many,
			comment => <<"test">>
		}]
	}),
	test_lib:leave_voicemail(Queue),
	test_lib:receive_voicemail().
