-module(t_core_basic).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can place a call to default queue"),
	A = test_lib:available(),
	{LegA, LegB} = ts_core:setup_talk(A),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),
	call:hangup(LegA),
	call:wait_hangup(LegB).