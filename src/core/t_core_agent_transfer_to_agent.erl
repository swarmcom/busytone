-module(t_core_agent_transfer_to_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to another agent"),
	[_Id, Queue] = admin:new_queue(#{
		wrapup_enabled => false
	}),
	A = test_lib:available(),
	ts_core:setup_talk(A, Queue),
	B = test_lib:available(),
	agent:rpc_call(A, transfer_to_agent, [B]),
	[UUID] = agent:wait_for_call(B),
	call:answer(UUID),
	agent:wait_ev(B, UUID, <<"CHANNEL_BRIDGE">>),
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"agent_id">> := A }, #{ <<"agent_id">> := B } ] = admin:agents_queue() end).
