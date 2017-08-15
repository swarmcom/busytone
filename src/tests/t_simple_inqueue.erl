-module(t_simple_inqueue).
-export([main/0]).

% test an agent recieve a call from default queue right after became available

main() ->
	{ok, InQueueCall} = call_sup:originate(<<"non_existent_queue">>),
	Agent = test_lib:available(admin:new_agent()),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	UUID = test_lib:answer(Agent),
	test_lib:ensureTalking(InQueueCall, UUID).