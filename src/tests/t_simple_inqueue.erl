-module(t_simple_inqueue).
-export([main/0]).

% test an agent recieve a call from default queue right after became available

main() ->
	call_sup:originate(<<"non_existent_queue">>),
	Agent = admin:new_agent(),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),
	test_lib:answer(Agent).
