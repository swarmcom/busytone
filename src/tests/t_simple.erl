-module(t_simple).
-export([main/0]).

% test an agent can login and receive calls from default queue

main() ->
	Agent = test_lib:available(admin:new_agent()),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	{ok, InQueueCall} = call_sup:originate(<<"non_existent_queue">>),
	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(InQueueCall, UUID),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"wrapup">>}).