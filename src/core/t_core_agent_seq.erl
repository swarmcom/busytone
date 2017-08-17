-module(t_core_agent_seq).
-export([main/0]).

% check agents are sorted by longest available state

main() ->
	A = test_lib:available(),
	B = test_lib:available(),
	C = test_lib:available(),
	[#{ <<"id">> := A }, #{ <<"id">> := B }, #{ <<"id">> := C } ] = admin:call(agents, []),

	test_lib:release(A),
	test_lib:available(A),
	timer:sleep(100),
	[#{ <<"id">> := B }, #{ <<"id">> := C }, #{ <<"id">> := A } ] = admin:call(agents, []).
