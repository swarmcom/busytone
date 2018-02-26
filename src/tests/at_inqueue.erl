-module(at_inqueue).
-export([main/0]).
-import(ts_core, [wait/1]).

-define(Number, <<"1234">>).

main() ->
	setup(),
	at_lib:check_inqueue_is_empty(),
	ExtCall = ts_make:call(?Number),
	{1, _X} = at_lib:wait_for_inqueue(?Number),
	call:hangup(ExtCall),
	at_lib:check_inqueue_is_empty().

setup() ->
	ts_make:dial_in().
