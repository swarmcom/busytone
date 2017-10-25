-module(t_core_agent_seq).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check agents are sorted by longest time in available state"),
	A = ts_make:available(),
	B = ts_make:available(),
	C = ts_make:available(),

	wait(fun() -> 
		[#{ <<"agent_id">> := A }, #{ <<"agent_id">> := B }, #{ <<"agent_id">> := C } ] = admin:agents_queue()
	end),

	ts_make:release(A),
	ts_make:available(A),

	wait(fun() -> 
		[#{ <<"agent_id">> := B }, #{ <<"agent_id">> := C }, #{ <<"agent_id">> := A } ] = admin:agents_queue()
	end).
