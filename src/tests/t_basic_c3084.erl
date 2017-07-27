-module(t_basic_c3084).
-export([main/0]).

% C3084: Client leaves a voicemail

main() ->
	Queue = admin:new_queue(#{
		recipe => []
	}),

	{ok, VmCall} = call_sup:originate(Queue),
	call:send_dtmf(VmCall, "*"),
	call:detect_tone(VmCall, "500"),
	call:wait_event(VmCall, <<"DETECTED_TONE">>),
	call:execute(VmCall, "playback", "tone_stream://%(1500, 0, 2600)"),
	timer:sleep(1500),
	call:hangup(VmCall),

	Agent = test_lib:available(admin:new_agent()),
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"event">> => <<"channel_playback_update">> }),
	test_lib:detect_tone(UUID, <<"2600">>).
