-module(test_simple).
-export([main/0]).

main() ->
	Agent = agent_sup:agent("agent1", "1234", "agent1"),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }, login_failure),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }, go_available_failure),
	{ok, UUID1} = call_manager:originate("sofia/gateway/reach/9999"),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"ringing">> }, no_incoming_call),
	[UUID] = agent:calls(Agent),
	ok = call:link_process(UUID1),
	ok = call:answer(UUID),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaevent">>,<<"event">> => <<"caller_offhold">>,<<"media">> => <<"voice">> }, no_offhold_message).

