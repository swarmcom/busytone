-module(t_voicemail_noanswer).
-export([main/0]).

main() ->
	Queue = admin:new_queue(#{
		recipe => [ #{
			conditions => [ [ticks, 1], [type, '=', voice] ],
			operations => [ [voicemail, []] ],
			runs => run_once,
			comment => <<"test">>
		}]
	}),
	test_lib:leave_voicemail(Queue),

	Agent = test_lib:available(admin:new_agent()),
	[UUID] = agent:wait_for_call(Agent),
	call:wait_hangup(UUID),

	test_lib:receive_voicemail(Agent).