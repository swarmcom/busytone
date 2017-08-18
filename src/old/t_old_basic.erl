-module(t_old_basic).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	A = ts_old:agent(agent2),
	test_lib:available(A),
	{ok, In} = call_sup:originate(<<"default_queue">>),
	[UUID] = agent:wait_for_call(A),
	ok = call:answer(UUID),
	call:hangup(UUID).
