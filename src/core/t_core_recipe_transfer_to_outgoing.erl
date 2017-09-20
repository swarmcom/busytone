-module(t_core_recipe_transfer_to_outgoing).
-export([main/0]).
-import(ts_core, [wait/1]).

main() ->
	lager:notice("check call recipe transfer to outgoing number action"),
	[Id, Queue] = admin:new_queue(#{
		skills => #{ german => true },
		recipe => [ #{
			conditions => [ [ticks, '=', 1] ],
			operations => [ [transfer_to_outgoing, <<"4000">>] ]
		}]
	}),
	_LineIn = admin:new_line_in(#{ queue_id => Id, number => Queue }),

	UUID = test_lib:originate(Queue),
	#{ <<"Unique-ID">> := UUID, <<"Caller-Destination-Number">> := Queue } = call_sup:wait_call(),
	#{ <<"Unique-ID">> := Out, <<"Caller-Destination-Number">> := <<"4000">> } = call_sup:wait_call(),
	call:wait_hangup(Out),
	call:hangup(UUID).