-module(t_routing_c3100).
-export([main/0]).

% C3100: Agent Failure treatment
% https://ezuce.testrail.com/index.php?/cases/view/3100

main() ->
	Profile = admin:new_profile(#{
		ring_timeout => 1,
		max_ring_fails => 1
	}),

	Queue = admin:new_queue(#{
		recipe => []
	}),

	Agent1 = test_lib:available(admin:new_agent(#{ profile => Profile })),
	timer:sleep(500),
	Agent2 = test_lib:available(admin:new_agent(#{ profile => Profile })),

	{ok, _InQueueCall} = call_sup:originate(Queue),

	[UUID1] = agent:wait_for_call(Agent1),
	call:wait_hangup(UUID1),

	[UUID2] = agent:wait_for_call(Agent2),
	call:wait_hangup(UUID2),

	{'EXIT', {timeout, _}} = (catch call_sup:wait_call()).