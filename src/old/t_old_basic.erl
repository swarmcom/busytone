-module(t_old_basic).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	A = ts_old:agent(agent1),
	test_lib:available(A),
	{ok, In} = call_sup:originate(<<"default_queue">>),
	[UUID] = agent:wait_for_call(A),
	ok = call:answer(UUID),
	timer:sleep(1000),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	timer:sleep(1000).