-module(t_seq_pickup).
-export([main/0]).

pickup(Agent, Ch, InCall) ->
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaload">>, <<"channelid">> => Ch }),
	test_lib:ensureTalking(UUID, InCall),
	call:hangup(UUID),
	call:wait_hangup(UUID).

main() ->
	{ok, InCall1} = call_sup:originate(<<"sofia/gateway/reach/9999">>),
	{ok, InCall2} = call_sup:originate(<<"sofia/gateway/reach/9999">>),
	Agent = admin:new_agent(),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),
	pickup(Agent, <<"ch1">>, InCall1),
	pickup(Agent, <<"ch2">>, InCall2).