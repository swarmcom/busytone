-module(t_core_adm).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin rpc: check agent queue and call queue returns"),
	A = admin:new_agent(),
	[] = admin:call(agents, []),
	test_lib:available(A),
	[#{ <<"login">> := A }] = admin:call(agents, []),
	agent:release(A),

	[] = admin:call(agents, []),
	[_Agent1, #{ <<"login">> := A }] = admin:call(agents, [release]),

	[] = admin:call(inqueues, []),
	UUID = call_sup:originate(<<"default_queue">>),
	wait(fun() -> [#{ <<"uuid">> := UUID }]  = admin:call(inqueues, []) end),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	wait(fun() -> [] = admin:call(inqueues, []) end).
