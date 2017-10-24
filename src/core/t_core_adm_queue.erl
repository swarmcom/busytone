-module(t_core_adm_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin api: create queue and place a call"),

	ts_core:dial_in(),

	UUID = test_lib:originate(<<"match">>),
	wait(fun() -> [#{ <<"uuid">> := UUID }] = admin:call(inqueues, []) end),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	wait(fun() -> [] = admin:call(inqueues, []) end).


