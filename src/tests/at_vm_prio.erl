-module(at_vm_prio).
-export([main/0]).
-import(ts_core, [wait/1]).

-define(Number1, <<"1001">>).
-define(Number2, <<"1002">>).

main() ->
	setup(),

	% lodge a call
	ts_make:call(?Number1, <<"1">>),
	{1, _} = at_lib:wait_for_inqueue(?Number1),

	leave_voicemail(?Number2, <<"2">>),
	timer:sleep(2000), % let recipe to kick in

	Agent = ts_make:available(),

	receive_voicemail(Agent, <<"2">>),
	receive_call(Agent, <<"1">>).

setup() ->
	Recipe = ts_make:recipe_with_entry(#{
		conditions => [ #{ name => type, args => ['=', <<"Voicemail">>] }],
		actions => [ #{ name => priority, args => [2] }]
	}),
	ts_make:dial_in(#{ queue => #{ recipe_id => Recipe }, line_in => #{ allow_voicemail => true } }).

leave_voicemail(Number, N) ->
	UUID = ts_make:call(Number, N),
	{2, _} = at_lib:wait_for_inqueue(Number),
	timer:sleep(1000), % call setup
	call:send_dtmf(UUID, "*"),
	timer:sleep(2000), % vm body
	call:hangup(UUID),
	timer:sleep(1000). % digest vm

receive_voicemail(Agent, N) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	call:wait_event(UUID, <<"CHANNEL_ANSWER">>),
	check_number(UUID, N),
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
