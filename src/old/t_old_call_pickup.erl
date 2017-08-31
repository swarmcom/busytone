-module(t_old_call_pickup).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	A = ts_old:agent(agent1),
	test_lib:available(A),
	In = call_sup:originate(<<"default_queue">>),
	[UUID] = agent:wait_for_call(A),
	timer:sleep(1000),
	ok = call:answer(UUID),
	timer:sleep(1000),
	test_lib:ensureTalking(UUID, In),
	test_lib:ensureTalking(In, UUID),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	timer:sleep(1000).