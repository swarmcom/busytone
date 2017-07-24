-module(t_voicemail).
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
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"event">> => <<"channel_playback_update">> }),
	test_lib:detect_tone(UUID, <<"2600">>).
