-module(t_core_supervisor_barge_spy).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("agent can spy to existing call, and can change spy modes"),
	Agent = test_lib:available(),
	{InqueueLeg, AgentLeg} = ts_core:setup_talk(Agent),
	Supervisor = admin:new_agent(),
	agent:rpc_call(Supervisor, ws_supervisor, barge, [inqueue_call, InqueueLeg]),
	[SupervisorLeg] = agent:wait_for_call(Supervisor),
	call:answer(SupervisorLeg),
	
	agent:wait_ws(Supervisor, #{ <<"event">> => <<"agent_state">>, <<"info">> => #{ <<"state">> => <<"barge">> } }),
	agent:rpc_call(Supervisor, ws_supervisor, set_barge_mode, [spy]),
	{'EXIT',{timeout,_}} = (catch test_lib:ensureTalking(SupervisorLeg, AgentLeg, 1000)),
	{'EXIT',{timeout,_}} = (catch test_lib:ensureTalking(SupervisorLeg, InqueueLeg, 1000)),
	call:hangup(InqueueLeg),
	call:hangup(AgentLeg),
	call:wait_hangup(SupervisorLeg).
