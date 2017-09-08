-module(ts_core).
-export([setup_talk/1, setup_talk/2, wait_agent_state/2, wait/1]).

% umbrella module for core tests

setup_talk(Agent) -> setup_talk(Agent, <<"default_queue">>).

setup_talk(Agent, Target) ->
	LegIn = test_lib:originate(Target),
	[LegAgent] = agent:wait_for_call(Agent),
	ok = call:answer(LegAgent),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),
	{LegIn, LegAgent}.

wait_agent_state(Agent, State) ->
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => State } }, 1).

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
