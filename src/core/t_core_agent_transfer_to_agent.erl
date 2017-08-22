-module(t_core_agent_transfer_to_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to another agent"),
	A = test_lib:available(),
	ts_core:setup_talk(A),
	B = test_lib:available(),
	agent:rpc_call(A, transfer_to_agent, [skip, B]),
	[UUID] = agent:wait_for_call(B),
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"login">> := A }, #{ <<"login">> := B } ] = admin:call(agents, []) end).
