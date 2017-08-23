-module(t_core_agent_out).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check outgoing call works for an agent"),
	A = test_lib:available(),
	agent:rpc_call(A, 'ouc.call_outbound', [some_destination]),
	[LegA] = agent:wait_for_call(A),
	ok = call:answer(LegA),
	#{ <<"Unique-ID">> := LegB, <<"Caller-Destination-Number">> := <<"some_destination">> } = call_sup:wait_call(),
	ok = call:answer(LegB),
	test_lib:ensureTalking(LegA, LegB),
	call:hangup(LegA),
	wait(fun() -> [ #{ <<"login">> := A } ] = admin:available_agents() end).
