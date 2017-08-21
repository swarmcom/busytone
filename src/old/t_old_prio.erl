-module(t_old_prio).
-export([main/0]).
-import(ts_core, [wait/1]).

answer(A, In) ->
	[UUID] = agent:wait_for_call(A),
	ok = call:answer(UUID),
	test_lib:ensureTalking(UUID, In),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	timer:sleep(1000).

main() ->
	A = ts_old:agent(agent1),
	{ok, In1} = call_sup:originate(<<"QueuePrio">>, [{origination_caller_id_number, <<"202">>}, {origination_caller_id_name, <<"202">>}]),
	{ok, In2} = call_sup:originate(<<"QueuePrio">>, [{origination_caller_id_number, <<"203">>}, {origination_caller_id_name, <<"203">>}]),
	timer:sleep(2000),
	test_lib:available(A),
	answer(A, In2),
	answer(A, In1).