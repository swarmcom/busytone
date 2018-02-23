-module(at_admin).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	Agent1 = ts_make:available(),
	Agent2 = ts_make:available().
