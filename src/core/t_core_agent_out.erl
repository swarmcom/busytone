-module(t_core_agent_out).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check outgoing call works for an agent"),
	A = test_lib:available(),
	agent:rpc_call(A, 'ouc.call_outbound', [some_destination]),
	[LegA] = agent:wait_for_call(A),
	ok = call:answer(LegA),
	#{ <<"Core-UUID">> := LegB } = call_sup:wait_call(),
	call:start_link(LegB),
	ok = call:answer(LegB),
	lager:error("~p", [LegB]),
	timer:sleep(10000).
