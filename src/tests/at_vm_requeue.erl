-module(at_vm_requeue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	setup(),

	leave_voicemail(),

	Agent1 = ts_make:available(),
	timer:sleep(1000),
	Agent2 = ts_make:available(),
	timer:sleep(1000), % make sure both are available

	reject_voicemail(Agent1),
	receive_voicemail(Agent2).

setup() ->
	ts_make:dial_in(#{ line_in => #{ allow_voicemail => true } }).

leave_voicemail() ->
	UUID = ts_make:call(<<"1234">>),
	timer:sleep(1000),
	call:send_dtmf(UUID, "*"),
	timer:sleep(2000),
	call:hangup(UUID),
	timer:sleep(1000). % digest vm

receive_voicemail(Agent) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, <<"CHANNEL_ANSWER">>),
	call:wait_hangup(UUID).

reject_voicemail(Agent) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:hangup(UUID).
