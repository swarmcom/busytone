-module(t_core_agent_seq).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check agents are sorted by longest time in available state"),
	A = test_lib:available(),
	B = test_lib:available(),
	C = test_lib:available(),
	wait(fun() -> [#{ <<"login">> := A }, #{ <<"login">> := B }, #{ <<"login">> := C } ] = admin:available_agents() end),

	test_lib:release(A),
	test_lib:available(A),
	wait(fun() -> [#{ <<"login">> := B }, #{ <<"login">> := C }, #{ <<"login">> := A } ] = admin:available_agents() end).
