-module(t_admin_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin can create a queue and place a call"),

	ts_make:dial_in(),

	UUID = test_lib:originate(<<"match">>),
	[#{ <<"uuid">> := _ReachUUID }] = wait(fun() -> [#{}] =admin:call(inqueues, []) end),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	wait(fun() -> [] = admin:call(inqueues, []) end).
