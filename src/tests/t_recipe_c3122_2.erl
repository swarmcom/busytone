-module(t_recipe_c3122_2).
-export([main/0]).

% C3122: Testing different recipe types, criterion based on tick interval, action is media announcement.

main() ->
	Queue = admin:new_queue(#{
		skills => #{ german => true },
		recipe => [ #{
			conditions => [ [ticks, 1] ],
			operations => [ [announce, <<"tone_stream://%(500, 0, 2600)">>] ],
			runs => run_many,
			comment => <<"test">>
		}]
	}),

	{ok, InQueueCall} = call_sup:originate(Queue),
	call:record(InQueueCall, start, "/tmp/record.wav"),
	test_lib:detect_tone(InQueueCall, <<"2600">>),
	timer:sleep(500), % wait signal to complete
	test_lib:detect_tone_now(InQueueCall, <<"2600">>).
