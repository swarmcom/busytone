-module(ts_core).
-export([setup_talk/1, setup_talk/2, wait_agent_state/2, wait/1]).
-export([ensure_talking/2, ensure_talking/3]).

% umbrella module for core tests

setup_talk(Agent) -> setup_talk(Agent, <<"default_queue">>).

setup_talk(Agent, Target) ->
	LegIn = test_lib:originate(Target),
	[LegAgent] = agent:wait_for_call(Agent),
	ok = call:answer(LegAgent),
	agent:wait_ev(Agent, LegAgent, <<"CHANNEL_BRIDGE">>),
	{LegIn, LegAgent}.

wait_agent_state(Agent, State) ->
	agent:wait_ws(Agent, #{ <<"event">> => <<"agent_state">>, <<"state">> => #{ <<"state">> => State } }, 1).

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

ensure_talking(UUID1, UUID2) -> ensure_talking(UUID1, UUID2, 5000).
ensure_talking(UUID1, UUID2, Timeout) ->
	call:execute(UUID1, "playback", "tone_stream://%(3000, 0, 2600)"),
	call:detect_tone(UUID2, "2600"),
	call:wait_event(UUID2, #{ <<"Event-Name">> => <<"DETECTED_TONE">> }, Timeout),
	call:stop_detect_tone(UUID2).
