-module(t_core_adm_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("admin api: create queue and place a call"),

	ClientId = admin:create(client),
	QueueId = admin:create(queue),
	LineInId = admin:create(line_in, #{ client_id => ClientId, queue_id => QueueId }),
	admin:create(dial, #{ match => <<".*">>, line_in_id => LineInId, header => <<"Caller-Destination-Number">> }),

	UUID = test_lib:originate(<<"match">>),
	wait(fun() -> [#{ <<"uuid">> := UUID }] = admin:call(inqueues, []) end),
	call:hangup(UUID),
	call:wait_hangup(UUID),
	wait(fun() -> [] = admin:call(inqueues, []) end).


