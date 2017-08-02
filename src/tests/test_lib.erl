-module(test_lib).
-export([
	login/3, available/1, hangup/1,
	answer/2, answer/1, ensureTalking/2, ensureTalking/3, detect_tone/2, detect_tone_now/2,
	leave_voicemail/1, leave_voicemail/2, receive_voicemail/0, receive_voicemail/1, vqueue_init/1
]).

% utility functions to automate tests

vqueue_init(Queue) ->
	{ok, UUID} = call_sup:originate(Queue),
	hangup(UUID),
	Queue.

leave_voicemail(Queue) ->
	{ok, VmCall} = call_sup:originate(Queue),
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

login(Login, Password, Number) ->
	Agent = agent_sup:agent(Login, Password, Number),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	Agent.

available(Agent) ->
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
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

answer(Agent) -> answer(Agent, <<"ch1">>).
answer(Agent, Ch) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, #{ <<"Event-Name">> => <<"CHANNEL_ANSWER">> }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"oncall">>, <<"channelid">> => Ch }),
	UUID.

ensureTalking(UUID1, UUID2) -> ensureTalking(UUID1, UUID2, 5000).
ensureTalking(UUID1, UUID2, Timeout) ->
	call:execute(UUID1, "playback", "tone_stream://%(3000, 0, 2600)"),
	call:detect_tone(UUID2, "2600"),
	call:wait_event(UUID2, #{ <<"Event-Name">> => <<"DETECTED_TONE">> }, Timeout),
	call:stop_detect_tone(UUID2).
