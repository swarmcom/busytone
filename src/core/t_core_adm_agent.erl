-module(t_core_adm_agent).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin api: create agent, make it release and available"),
	AgentId = admin:create(agent),
	[] = admin:call(agents, []),
	test_lib:available(AgentId),
	[#{ <<"agent_id">> := AgentId }] = admin:call(agents, []),

	agent:release(AgentId),
	wait(fun() -> [] = admin:call(agents, []) end),

	[_Agent1, #{ <<"agent_id">> := AgentId }] = admin:call(agents, [release]).
