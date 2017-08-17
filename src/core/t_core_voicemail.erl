-module(t_core_voicemail).
-export([main/0]).

% check call conversion to voicemail
main() ->
	lager:notice("check inqueue call conversion to voicemail"),
	A = admin:new_agent(),
	{ok, In} = call_sup:originate("default_queue"),
	admin:call(subscribe, [uuid, In]),
	[#{ <<"uuid">> := In, <<"state">> := <<"inqueue">>, <<"type">> := <<"call">> }]  = admin:call(inqueues, []),
	timer:sleep(100),
	call:send_dtmf(In, "*"),
	admin:wait_ws(#{ <<"Application">> => <<"sleep">>, <<"Unique-ID">> => In }),
	timer:sleep(500),
	call:hangup(In),
	[#{ <<"uuid">> := In, <<"state">> := <<"inqueue">>, <<"type">> := <<"voicemail">> }]  = admin:call(inqueues, []),

	agent:available(A),
	[LegA] = agent:wait_for_call(A),
	ok = call:answer(LegA),
	call:wait_hangup(LegA),
	[]  = admin:call(inqueues, []).
