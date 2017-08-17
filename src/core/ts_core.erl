-module(ts_core).
-export([setup_talk/1]).

% umbrella module for core tests

setup_talk(Agent) ->
	{ok, LegA} = call_sup:originate(<<"default_queue">>),
	[LegB] = agent:wait_for_call(Agent),
	ok = call:answer(LegB),
	agent:wait_ev(Agent, LegB, <<"CHANNEL_BRIDGE">>),
	{LegA, LegB}.