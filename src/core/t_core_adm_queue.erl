-module(t_core_adm_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin rpc: create agent, make it release and available"),
	A = admin:new_agent(),
	[] = admin:call(agents, []),
	test_lib:available(A),
	[#{ <<"id">> := A }] = admin:call(agents, []),

	agent:release(A),
	wait(fun() -> [] = admin:call(agents, []) end),

	[_Agent1, #{ <<"agent_id">> := A }] = admin:call(agents, [release]).
