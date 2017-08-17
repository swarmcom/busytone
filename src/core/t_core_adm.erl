-module(t_core_adm).
-export([main/0]).

% test admin rpc: check agent queue and call queue returns

main() ->
	A = admin:new_agent(),
	[] = admin:call(agents, []),
	test_lib:available(A),
	[#{ <<"login">> := A }] = admin:call(agents, []),
	agent:release(A),
	[] = admin:call(agents, []),
	[_Agent1, #{ <<"login">> := A }] = admin:call(agents, [release]),

	[] = admin:call(inqueues, []),
	{ok, UUID} = call_sup:originate(<<"default_queue">>),
	[#{ <<"uuid">> := UUID }]  = admin:call(inqueues, []),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	timer:sleep(500),
	[] = admin:call(inqueues, []).
