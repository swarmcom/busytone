-module(t_core_inqueue_voicemail).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("inqueue call goes to voicemail by dialing *, and agent can receive it"),

	ts_make:dial_in(#{ line_in => #{ allow_voicemail => true }}),
	In = ts_make:call(whatever),

	admin:call(subscribe, [event, <<"CHANNEL_EXECUTE">>]),
	wait(fun() -> [#{ <<"uuid">> := In, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_call">> }]  = admin:call(inqueues, []) end),
	call:send_dtmf(In, "*"),
	admin:wait_ws(#{ <<"call_event">> => <<"CHANNEL_EXECUTE">>, <<"vars">> => #{ <<"Application">> => <<"sleep">>, <<"Unique-ID">> => In }}),
	timer:sleep(2000), % voicemail body
	call:hangup(In),
	wait(fun() -> [#{ <<"uuid">> := In, <<"state">> := <<"inqueue">>, <<"record">> := <<"inqueue_vm">> }]  = admin:call(inqueues, []) end),

	timer:sleep(1000), % need to wait vm to get stored

	A = ts_make:available(),
	[LegA] = agent:wait_for_call(A),
	ok = call:answer(LegA),
	call:wait_event(LegA, <<"CHANNEL_ANSWER">>),
	call:wait_hangup(LegA),
	wait(fun() -> []  = admin:call(inqueues, []) end),
	[#{ <<"agent_id">> := A, <<"state">> := <<"available">> }] = admin:agents_queue(),
	admin:call(unsubscribe, [event, <<"CHANNEL_EXECUTE">>]).
