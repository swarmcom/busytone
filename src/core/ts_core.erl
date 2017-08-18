-module(ts_core).
-export([setup_talk/1, wait/1]).

% umbrella module for core tests

setup_talk(Agent) ->
	{ok, LegA} = call_sup:originate(<<"default_queue">>),
	[LegB] = agent:wait_for_call(Agent),
	ok = call:answer(LegB),
	agent:wait_ev(Agent, LegB, <<"CHANNEL_BRIDGE">>),
	{LegA, LegB}.

wait(F) -> maybe_wait(F, 5000).

maybe_wait(_F, T) when T =< 0 -> erlang:error(maybe_wait_failure);
maybe_wait(F, T) ->
	try
		F()
	catch
		_:_ ->
			timer:sleep(100),
			maybe_wait(F, T-100)
	end.
