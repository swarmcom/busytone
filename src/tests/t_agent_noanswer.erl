-module(t_agent_noanswer).
-export([main/0]).

main() ->
	Agent = admin:new_agent(),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	{ok, InQueueCall} = call_sup:originate(<<"sofia/gateway/reach/9999">>),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">>, <<"channelid">> => <<"ch1">> }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"endchannel">>, <<"channelid">> => <<"ch1">> }, 15000),
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"oncall">>, <<"channelid">> => <<"ch2">> }),
	test_lib:ensureTalking(InQueueCall, UUID).
