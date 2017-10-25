-module(t_admin_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin can create agent and make it release/available"),
	AgentId = admin:create(agent),
	[] = admin:call(agents, []),
	test_lib:available(AgentId),
	[#{ <<"agent_id">> := AgentId }] = admin:call(agents, []),

	agent:release(AgentId),
	wait(fun() -> [] = admin:call(agents, []) end),

	[_Agent1, #{ <<"agent_id">> := AgentId }] = admin:call(agents, [release]).
