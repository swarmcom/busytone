-module(t_core_agent_transfer_to_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can transfer a call to another agent"),
	[Id, Queue] = admin:new_queue(#{
		wrapup_enabled => false
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),
	A = ts_make:available(),
	ts_core:setup_talk(A, Queue),
	B = ts_make:available(),
	agent:call(A, transfer_to_agent, [B]),
	[UUID] = agent:wait_for_call(B),
	call:answer(UUID),
	agent:wait_ev(B, UUID, <<"CHANNEL_BRIDGE">>),
	call:hangup(UUID),
	wait(fun() -> [ #{ <<"agent_id">> := A }, #{ <<"agent_id">> := B } ] = admin:agents_queue() end).
