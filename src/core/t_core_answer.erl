-module(t_core_answer).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check call is answered by agent, and hangup works for both legs"),

	ts_make:dial_in(),

	A = ts_make:available(),
	test_hup_a(A),
	ts_make:release(A),

	B = ts_make:available(),
	test_hup_b(B),
	wait(fun() -> [#{ <<"state">> := <<"available">> }] = admin:agents_queue() end).

setup_talk(A) ->
	LegA = ts_make:call(whatever),
	[LegB] = agent:wait_for_call(A),
	ok = call:answer(LegB),
	agent:wait_ev(A, LegB, <<"CHANNEL_BRIDGE">>),
	wait(fun() -> [#{ <<"agent_id">> := A, <<"uuid">> := LegB, <<"inqueue">> := #{ <<"inqueue_call">> := LegA } }] = admin:call(agents, [oncall]) end),
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
