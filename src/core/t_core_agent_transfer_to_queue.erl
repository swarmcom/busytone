-module(t_core_agent_transfer_to_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to a queue"),
	A = test_lib:available(),
	ts_core:setup_talk(A),
	B = test_lib:available(),
	agent:rpc_call(A, transfer_to_queue, [skip, <<"default_queue">>, []]),
	[UUID] = agent:wait_for_call(B), % it should be B-agent to get a call, as it has longest wait time in available state
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"login">> := A }, #{ <<"login">> := B } ] = admin:available_agents() end).
