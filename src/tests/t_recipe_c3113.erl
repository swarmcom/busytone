-module(t_recipe_c3113).
-export([main/0]).

% C3113: Calls in queue >2, send to VM

dequeue(UUID) ->
	call:hangup(UUID),
	call:wait_hangup(UUID).

main() ->
	Queue = admin:new_queue(#{
		recipe => [ #{
			conditions => [ [calls_queued, '>', 2] ],
			operations => [ [voicemail, []] ],
			runs => run_once,
			comment => <<"test">>
		}]
	}),
	{ok, UUID1} = call_sup:originate(Queue),
	{ok, UUID2} = call_sup:originate(Queue),
	{ok, UUID3} = call_sup:originate(Queue),

	test_lib:leave_voicemail(UUID1, call),

	% cleanup (get the voicemail)
	dequeue(UUID2),
	dequeue(UUID3),

	test_lib:receive_voicemail().
