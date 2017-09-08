-module(t_core_inqueue_hup).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check inqueue record is bound to call"),
	A = test_lib:originate(<<"default_queue">>),
	wait(fun() -> [#{ <<"uuid">> := A, <<"state">> := <<"inqueue">> }] = admin:call(inqueues, []) end),
	call:hangup(A),
	call:wait_hangup(A),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
