-module(t_voicemail).
-export([main/0]).

% leave a voicemail, and an logged-in agent gets it

main() ->
	Queue = admin:new_queue(),

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
	test_lib:detect_tone(UUID, <<"2600">>).
