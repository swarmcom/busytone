-module(t_core_answer).
-export([main/0]).

% check call is answered by agent, and hangup works for both legs

setup_talk(A) ->
	{ok, LegA} = call_sup:originate(<<"default_queue">>),
	[LegB] = agent:wait_for_call(A),
	ok = call:answer(LegB),
	agent:wait_ws(A, #{ <<"command">> => <<"setchannel">>, <<"state">> => <<"oncall">> }),
	[#{ <<"id">> := A, <<"hangup_state">> := <<"available">>, <<"uuid">> := LegB, <<"inqueue">> := InM }] = admin:call(agents, [oncall]),
	#{ <<"inqueue">> := LegA } = InM,
	[#{ <<"state">> := <<"oncall">> }] = admin:call(inqueues, [oncall]),
	{LegA, LegB}.

test_hup_a(A) ->
	{LegA, _LegB} = setup_talk(A),
	test_lib:hangup(LegA),
	timer:sleep(100),
	[] = admin:call(inqueues, [oncall]).

test_hup_b(A) ->
	{_LegA, LegB} = setup_talk(A),
	test_lib:hangup(LegB),
	timer:sleep(100),
	[] = admin:call(inqueues, [oncall]).

main() ->
	A = test_lib:available(),
	test_hup_a(A),
	test_lib:release(A),

	B = test_lib:available(),
	test_hup_b(B),
	[#{ <<"state">> := <<"available">> }] = admin:call(agents, []).


