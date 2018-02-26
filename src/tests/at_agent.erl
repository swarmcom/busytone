-module(at_agent).
-export([main/0]).

main() ->
	Agent1 = ts_make:available(),
	Agent2 = ts_make:available(),
	{1, _} = at_lib:wait_for_agent(Agent1, <<"available">>),
	{2, _} = at_lib:wait_for_agent(Agent2, <<"available">>).
