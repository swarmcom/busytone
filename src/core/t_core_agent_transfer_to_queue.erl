-module(t_core_agent_transfer_to_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to a queue"),
	A = test_lib:available(),
	ts_core:setup_talk(A),
	B = test_lib:available(),
	#{ <<"id">> := QueueId } = agent:rpc_call(A, ws_admin, get_queue_by_name, [<<"default_queue">>]),
	agent:rpc_call(A, transfer_to_queue, [QueueId, []]),
	[UUID] = agent:wait_for_call(B), % it should be B-agent to get a call, as it has longest wait time in available state
	call:answer(UUID),
	agent:wait_ev(B, UUID, <<"CHANNEL_BRIDGE">>),
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"agent_id">> := A }, #{ <<"agent_id">> := B } ] = admin:agents_queue() end).
