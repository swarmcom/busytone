-module(t_old_vm).
-export([main/0]).
-import(ts_core, [wait/1]).

leave_vm() ->
	{ok, In} = call_sup:originate(<<"QueueVM">>),
	timer:sleep(1000),
	call:send_dtmf(In, <<"*">>),
	timer:sleep(1000),
	call:hangup(In).

main() ->
	A = ts_old:agent(agent2),

	leave_vm(),

	test_lib:available(A),

	[UUID] = agent:wait_for_call(A),
	call:answer(UUID),
	timer:sleep(1000),
	call:hangup(UUID),

	leave_vm(),

	[UUID1] = agent:wait_for_call(A),
	call:answer(UUID1),
	timer:sleep(1000),
	call:hangup(UUID1),
	timer:sleep(1000).
