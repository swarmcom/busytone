-module(test_seq_pickup).
-export([main/0]).

main() ->
	call_manager:originate("sofia/gateway/reach/9999"),
	call_manager:originate("sofia/gateway/reach/9999"),
	Agent = agent_sup:agent("agent1", "1234", "agent1"),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }),

	[UUID1] = agent:wait_for_call(Agent),
	ok = call:answer(UUID1),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaevent">>,<<"event">> => <<"caller_offhold">>,<<"media">> => <<"voice">> }),
	call:hangup(UUID1),
	call:wait_hangup(UUID1),

	[UUID2] = agent:wait_for_call(Agent),
	ok = call:answer(UUID2),
	agent:wait_ws(Agent, #{<<"channelid">> => <<"ch2">>, <<"command">> => <<"mediaload">> }),
	call:hangup(UUID2).
