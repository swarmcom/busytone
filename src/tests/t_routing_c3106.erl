-module(t_routing_c3106).
-export([main/0]).

% C3106: Priority routing - queue weight
% https://ezuce.testrail.com/index.php?/cases/view/3106

main() ->
	Queue1 = admin:new_queue(#{ weight => 1, recipe => [], wrapup_enabled => false }),
	Queue2 = admin:new_queue(#{ weight => 3, recipe => [], wrapup_enabled => false }),
	Queue3 = admin:new_queue(#{ weight => 6, recipe => [], wrapup_enabled => false }),

	{ok, InCall1} = call_sup:originate(Queue1),
	{ok, InCall2} = call_sup:originate(Queue2),
	{ok, InCall3} = call_sup:originate(Queue3),

	Agent = test_lib:available(admin:new_agent(#{})),

	UUID1 = test_lib:answer(Agent, <<"ch1">>),
	test_lib:ensureTalking(UUID1, InCall3),
	test_lib:hangup(UUID1),

	UUID2 = test_lib:answer(Agent, <<"ch2">>),
	test_lib:ensureTalking(UUID2, InCall2),
	test_lib:hangup(UUID2),

	UUID3 = test_lib:answer(Agent, <<"ch3">>),
	test_lib:ensureTalking(UUID3, InCall1),
	test_lib:hangup(UUID3).
