-module(t_core_supervisor_spy_mode).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and can change spy modes"),
	Agent = test_lib:available(),
	{InqueueLeg, AgentLeg} = ts_core:setup_talk(Agent),
	Supervisor = admin:new_agent(),
	agent:rpc_call(Supervisor, ws_supervisor, spy, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),
	agent:wait_ws(Supervisor, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"barge">> } }),

	{'EXIT',{timeout,_}} = (catch ts_core:ensure_talking(SupervisorLeg, AgentLeg, 1000)),
	agent:rpc_call(Supervisor, ws_supervisor, set_barge_mode, [agent]),
	ts_core:ensure_talking(SupervisorLeg, AgentLeg),

	{'EXIT',{timeout,_}} = (catch ts_core:ensure_talking(SupervisorLeg, InqueueLeg, 1000)),
	agent:rpc_call(Supervisor, ws_supervisor, set_barge_mode, [caller]),
	ts_core:ensure_talking(SupervisorLeg, InqueueLeg),

	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
