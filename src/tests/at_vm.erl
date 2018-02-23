-module(at_vm).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	setup(),
	leave_voicemail(),
	receive_voicemail().

setup() ->
	ts_make:dial_in( #{ line_in => #{ allow_voicemail => true } }).

leave_voicemail() ->
	UUID = ts_make:call("1234"),
	[#{ <<"uuid">> := InQ }] = wait(fun() -> [#{ <<"state">> := <<"inqueue">> }]  = admin:call(inqueues, []) end),
	call:send_dtmf(UUID, "*"),
	timer:sleep(2000),
	call:hangup(UUID),
	timer:sleep(1000). % digest vm

receive_voicemail() ->
	Agent = ts_make:available(),
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, <<"CHANNEL_ANSWER">>),
	call:wait_hangup(UUID).
