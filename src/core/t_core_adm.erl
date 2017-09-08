-module(t_core_adm).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin rpc: check agent queue and call queue returns"),
	A = admin:new_agent(),
	[] = admin:call(agents, []),
	test_lib:available(A),
	[#{ <<"agent_id">> := A }] = admin:call(agents, []),

	agent:release(A),
	wait(fun() -> [] = admin:call(agents, []) end),

	[_Agent1, #{ <<"agent_id">> := A }] = admin:call(agents, [release]),

	[] = admin:call(inqueues, []),
	UUID = test_lib:originate(<<"default_queue">>),
	wait(fun() -> [#{ <<"uuid">> := UUID }] = admin:call(inqueues, []) end),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	wait(fun() -> [] = admin:call(inqueues, []) end).
