-module(ts_old).
-export([agent/1]).
-import(ts_core, [wait/1]).

% umbrella module to test 17.08 branch

agents() ->
	#{
		agent1 => [<<"agent1">>, <<"1234">>, <<"agent1">>],
		agent2 => [<<"agent2">>, <<"1234">>, <<"agent2">>],
		agent3 => [<<"agent3">>, <<"1234">>, <<"agent3">>]
	}.

agent(Id) ->
	Rec = maps:get(Id, agents()),
	A = erlang:apply(agent_sup, agent, Rec),
	wait(fun() -> ws = agent:auth(A) end),
	A.
