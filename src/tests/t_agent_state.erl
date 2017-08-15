-module(t_agent_state).
-export([main/0]).

% test an agent can login and receive calls from default queue

main() ->
	test1(),
	test2().

test1() ->
	Agent = test_lib:available(admin:new_agent()),
	{ok, InQueueCall} = call_sup:originate(<<"default_queue">>),
	UUID = test_lib:answer(Agent),
	call:hangup(UUID),
	call:wait_hangup(InQueueCall).

test2() ->
	Agent = test_lib:available(admin:new_agent()),
	{ok, InQueueCall} = call_sup:originate(<<"default_queue">>),
	UUID = test_lib:answer(Agent),
	call:hangup(InQueueCall),
	call:wait_hangup(UUID).