-module(test_lib).
-import(ts_core, [wait/1]).
-export([
	available/0, available/1, release/1, hangup/1, originate/1,
	answer/1, ensureTalking/2, ensureTalking/3, detect_tone/2, detect_tone_now/2,
	leave_voicemail/1, leave_voicemail/2, receive_voicemail/0, receive_voicemail/1, vqueue_init/1
]).

% utility functions to automate tests

originate(Target) ->
	UUID = call_sup:originate(Target),
	call:wait(UUID).

vqueue_init(Queue) ->
	{ok, UUID} = test_lib:originate(Queue),
	hangup(UUID),
	Queue.

leave_voicemail(Queue) ->
	{ok, VmCall} = test_lib:originate(Queue),
	leave_voicemail(VmCall, call).

leave_voicemail(VmCall, call) ->
	call:detect_tone(VmCall, "500"),
	call:wait_event(VmCall, <<"DETECTED_TONE">>),
	call:execute(VmCall, "playback", "tone_stream://%(1500, 0, 2600)"),
	timer:sleep(1500),
	call:send_dtmf(VmCall, "*"),
	timer:sleep(500),
	call:hangup(VmCall).

receive_voicemail() ->
	receive_voicemail(available(admin:new_agent())).

receive_voicemail(Agent) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"event">> => <<"channel_playback_update">> }),
	test_lib:detect_tone(UUID, <<"2600">>),
	{Agent, UUID}.

available() -> available(admin:new_agent()).

available(Agent) ->
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"hangup_state">> => <<"available">> } }),
	Agent.

release(Agent) ->
	agent:release(Agent),
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"hangup_state">> => <<"available">> } }),
	Agent.

detect_tone(UUID, Tone) ->
	call:detect_tone(UUID, Tone),
	call:wait_event(UUID, <<"DETECTED_TONE">>),
	call:stop_detect_tone(UUID).

detect_tone_now(UUID, Tone) ->
	call:detect_tone(UUID, Tone),
	call:wait_event_now(UUID, <<"DETECTED_TONE">>),
	call:stop_detect_tone(UUID).

hangup(UUID) ->
	call:hangup(UUID),
	call:wait_hangup(UUID).

answer(Agent) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, #{ <<"Event-Name">> => <<"CHANNEL_ANSWER">> }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"oncall">> }),
	UUID.

ensureTalking(UUID1, UUID2) -> ensureTalking(UUID1, UUID2, 5000).
ensureTalking(UUID1, UUID2, Timeout) ->
	call:execute(UUID1, "playback", "tone_stream://%(3000, 0, 2600)"),
	call:detect_tone(UUID2, "2600"),
	call:wait_event(UUID2, #{ <<"Event-Name">> => <<"DETECTED_TONE">> }, Timeout),
	call:stop_detect_tone(UUID2).
