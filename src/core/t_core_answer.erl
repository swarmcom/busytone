-module(t_core_answer).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check call is answered by agent, and hangup works for both legs"),
	A = test_lib:available(),
	test_hup_a(A),
	test_lib:release(A),

	B = test_lib:available(),
	test_hup_b(B),
	wait(fun() -> [#{ <<"state">> := <<"available">> }] = admin:call(agents, []) end).

setup_talk(A) ->
	{ok, LegA} = call_sup:originate(<<"default_queue">>),
	[LegB] = agent:wait_for_call(A),
	ok = call:answer(LegB),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),
	wait(fun() -> [#{ <<"login">> := A, <<"uuid">> := LegB, <<"inqueue">> := #{ <<"inqueue_call">> := LegA } }] = admin:call(agents, [oncall]) end),
	[#{ <<"state">> := <<"oncall">> }] = admin:call(inqueues, [oncall]),
	{LegA, LegB}.

test_hup_a(A) ->
	{LegA, LegB} = setup_talk(A),
	test_lib:hangup(LegA),
	agent:wait_ev(A, LegA, <<"CHANNEL_HANGUP">>),
	agent:wait_ev(A, LegB, <<"CHANNEL_HANGUP">>),
	wait(fun() -> [] = admin:call(inqueues, [oncall]) end).

test_hup_b(A) ->
	{LegA, LegB} = setup_talk(A),
	test_lib:hangup(LegB),
	agent:wait_ev(A, LegB, <<"CHANNEL_HANGUP">>),
	agent:wait_ev(A, LegA, <<"CHANNEL_HANGUP">>),
	wait(fun() -> [] = admin:call(inqueues, [oncall]) end).
