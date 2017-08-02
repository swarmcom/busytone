-module(t_vm_transfer).
-export([main/0]).

% test an agent can transfer a voicemail call back to the queue

main() ->
	Queue = admin:new_queue(#{
		recipe => [ #{
			conditions => [ [ticks, 1], [type, '=', voice] ],
			operations => [ [voicemail, []] ],
			runs => run_once,
			comment => <<"test">>
		}]
	}),
	test_lib:vqueue_init(Queue), % XXX: wtf?
	test_lib:leave_voicemail(Queue),

	Agent = test_lib:available(admin:new_agent()),
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"event">> => <<"channel_playback_update">> }),
	agent:rpc(Agent, <<"transfer_to_queue">>, [<<"ch1">>, Queue, #{ skills => [] }]),
	call:wait_hangup(UUID),
	agent:rpc_call(Agent, <<"end_wrapup">>, [<<"ch1">>]),

	[UUID1] = agent:wait_for_call(Agent),
	ok = call:answer(UUID1),
	test_lib:detect_tone(UUID1, <<"2600">>).