-module(t_core_agent_seq).
-export([main/0]).

% check agents are sorted by longest available state

main() ->
	A = test_lib:available(),
	B = test_lib:available(),
	C = test_lib:available(),
	[#{ <<"login">> := A }, #{ <<"login">> := B }, #{ <<"login">> := C } ] = admin:call(agents, []),

	test_lib:release(A),
	test_lib:available(A),
	timer:sleep(100),
	[#{ <<"login">> := B }, #{ <<"login">> := C }, #{ <<"login">> := A } ] = admin:call(agents, []).
