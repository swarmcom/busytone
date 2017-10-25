-module(t_core_inqueue_hup).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("inqueue process is bound to call"),
	ts_make:dial_in(),
	UUID = ts_make:call(whatever),
	wait(fun() -> [#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">> }] = admin:call(inqueues, []) end),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	wait(fun() -> [] = admin:call(inqueues, [all]) end).
