-module(test_simple).
-export([main/0]).

main() ->
	Agent = agent_sup:agent("agent1", "1234", "1001"),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }, 5000, ws_login_failure).