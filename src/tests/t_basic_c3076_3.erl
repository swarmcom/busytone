-module(t_basic_c3076_3).
-export([main/0]).

% C3076: Wrap-up enabled, auto-wrapup is set

main() ->
	Queue = admin:new_queue(#{
		skills => #{ english => true },
		recipe => [],
		wrapup_enabled => true,
		auto_wrapup => 500
	}),

	Agent = test_lib:available(admin:new_agent( #{ skills => #{ english => true } })),
	{ok, _} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaevent">>, <<"event">> => <<"caller_offhold">> }),
	agent:rpc_call(Agent, <<"hangup">>, [UUID]),
	agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>,<<"state">> => <<"wrapup">> }),
	agent:wait_ws(Agent, #{ <<"command">> => <<"endchannel">> }).
