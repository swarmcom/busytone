-module(t_seq_pickup).
-export([main/0]).

% test sequential pickup: and agent should be able to sequentially pickup calls in order they get into a queue

pickup(Agent, Ch, InCall) ->
	UUID = test_lib:answer(Agent, Ch),
	test_lib:ensureTalking(UUID, InCall),
	call:hangup(UUID),
	call:wait_hangup(UUID).

main() ->
	{ok, InCall1} = call_sup:originate(<<"default_queue">>),
	{ok, InCall2} = call_sup:originate(<<"default_queue">>),
	Agent = test_lib:available(admin:new_agent()),
	pickup(Agent, <<"ch1">>, InCall1),
	pickup(Agent, <<"ch2">>, InCall2).