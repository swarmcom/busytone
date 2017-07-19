-module(t_seq_login).
-export([main/0]).

login() ->
	Agent = admin:new_agent(),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	agent:available(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"arelease">>, <<"releaseData">> => false }),
	agent:stop(Agent).

main() ->
	login(),
	login().