-module(at_lib).
-import(ts_core, [wait/1]).

-export([
	check_inqueue_is_empty/0,
	wait_for_inqueue/1,
	wait_for_agent/1,
	wait_for_agent/2
]).

check_inqueue_is_empty() ->
	wait(fun() -> [] = admin:call(inqueues, []) end).

wait_for_inqueue(Number) ->
	wait_for(fun(Inqueue) -> match_caller_dest(Inqueue, Number) end, fun() -> admin:call(inqueues, []) end).

match_caller_dest(#{ <<"call_vars">> := #{ <<"Caller-Destination-Number">> := N }}, Number) -> N =:= Number.

match_agent_id(#{ <<"agent_id">> := Id }, AgentId) -> AgentId =:= Id;
match_agent_id(_, _) -> false.

match_agent_id_state(#{ <<"agent_id">> := Id, <<"state">> := S }, AgentId, State) -> AgentId =:= Id andalso State =:= S;
match_agent_id_state(_, _, _) -> false.

wait_for_agent(AgentId) ->
	wait_for(fun(Agent) -> match_agent_id(Agent, AgentId) end, fun() -> admin:call(agents, []) end).

wait_for_agent(AgentId, State) ->
	wait_for(fun(Agent) -> match_agent_id_state(Agent, AgentId, State) end, fun() -> admin:call(agents, []) end).

wait_for(Match, Query) ->
	wait(
		fun() ->
			{_, [Re]} = lists:foldl(
				fun(Agent, {N,Acc}) ->
					case Match(Agent) of
						true -> {N+1, [{N, Agent} | Acc ]};
						_ -> {N+1, Acc}
					end
				end,
				{1, []},
				Query()
			),
			Re
	end).
