-module(t_core_recipe_transfer_to_queue).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check call recipe transfer to queue action"),
	[QueueId, _] = admin:new_queue(),
	[Id, Queue] = admin:new_queue(#{
		skills => #{ german => true },
		recipe => [ #{
			conditions => [ [ticks, '=', 1] ],
			operations => [ [transfer_to_queue, QueueId] ]
		}]
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),

	UUID = test_lib:originate(Queue),
	wait(fun() ->
		[#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"queue_id">> := Id }] = admin:call(inqueues, [])
	end),

	wait(fun() ->
		[#{ <<"uuid">> := UUID, <<"state">> := <<"inqueue">>, <<"queue_id">> := QueueId }] = admin:call(inqueues, [])
	end),

	ok = call:hangup(UUID).