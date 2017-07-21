-module(t_rpc_queue).
-export([main/0]).

main() ->
	Group = admin:new_group(),
	#{ <<"name">> := <<"test_queue_group_1">> } = admin:get_group(Group),
	Queue = admin:new_queue(#{ group => Group }),
	#{ <<"name">> := <<"test_queue_1">>, <<"group">> := Group } = admin:get_queue(Queue).
