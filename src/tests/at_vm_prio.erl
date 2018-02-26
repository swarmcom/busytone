-module(at_vm_prio).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	setup(),

	% lodge a call
	ts_make:call(<<"1234">>, <<"1">>),
	timer:sleep(1000),

	leave_voicemail(<<"2">>),
	timer:sleep(3000), % let recipe to kick in

	Agent = ts_make:available(),

	receive_voicemail(Agent, <<"2">>),
	receive_call(Agent, <<"1">>).

setup() ->
	Recipe = ts_make:recipe_with_entry(#{
		conditions => [ #{ name => type, args => ['=', <<"Voicemail">>] }],
		actions => [ #{ name => priority, args => [2] }]
	}),
	ts_make:dial_in(#{ queue => #{ recipe_id => Recipe }, line_in => #{ allow_voicemail => true } }).

leave_voicemail(N) ->
	UUID = ts_make:call(<<"1234">>, N),
	timer:sleep(1000),
	call:send_dtmf(UUID, "*"),
	timer:sleep(2000),
	call:hangup(UUID),
	timer:sleep(1000). % digest vm

receive_voicemail(Agent, _N) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, <<"CHANNEL_ANSWER">>),
	call:wait_hangup(UUID).

receive_call(Agent, N) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, <<"CHANNEL_ANSWER">>),
	check_number(UUID, N),
	timer:sleep(2000),
	call:hangup(UUID).

check_number(UUID, Number) ->
	[M] = call:vars(UUID),
	Number = maps:get(<<"Caller-Caller-ID-Number">>, M).
