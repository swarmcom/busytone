-module(t_core_queue_by_line).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can place a call to the specific queue"),
	[QueueId, _] = admin:new_queue(),
	_LineIn = admin:new_line_in(#{ number => <<"4000">>, queue_id => QueueId }),
	A = test_lib:available(),
	{LegIn, LegAgent} = ts_core:setup_talk(A, <<"4000">>),
	[#{ <<"queue_id">> := QueueId }] = admin:call(inqueues, [all]),
	agent:wait_ev(A, LegAgent, <<"CHANNEL_BRIDGE">>),
	call:hangup(LegIn),
	call:wait_hangup(LegAgent).