-module(t_agent_noanswer).
-export([main/0]).

main() ->
	Agent = admin:new_agent( #{ skills => #{ english => true }, ring_timeout => 3 }),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	{ok, InQueueCall} = call_sup:originate(<<"9999">>),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">>, <<"channelid">> => <<"ch1">> }),
	[UUID1] = agent:wait_for_call(Agent),
	call:wait_hangup(UUID1),
	UUID = test_lib:answer(Agent, <<"ch2">>),
	test_lib:ensureTalking(InQueueCall, UUID).
