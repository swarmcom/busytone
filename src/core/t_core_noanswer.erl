-module(t_core_noanswer).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("call gets in queue, then drops"),
	_Agent = test_lib:available(),
	UUID = test_lib:originate(<<"default_queue">>),
	#{ <<"Unique-ID">> := AgentLeg } = call_sup:wait_call(),
	call:hangup(UUID),
	call:hangup(AgentLeg),
	timer:sleep(1000).