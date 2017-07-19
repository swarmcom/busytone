-module(t_simple_inqueue).
-export([main/0]).

main() ->
	call_sup:originate(<<"sofia/gateway/reach/9999">>),
	Agent = admin:new_agent(),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),
	[UUID] = agent:wait_for_call(Agent),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaload">>, <<"channelid">> => <<"ch1">> }).
