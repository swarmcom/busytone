-module(t_basic_c3076_2).
-export([main/0]).

% C3076: Wrap-up enabled, timer is set

main() ->
	Queue = admin:new_queue(#{
		skills => #{ english => true },
		recipe => [],
		wrapup_enabled => true,
		wrapup_timer => 1000
	}),

	Agent = test_lib:available(admin:new_agent( #{ skills => #{ english => true } })),
	{ok, _} = call_sup:originate(Queue),
	UUID = test_lib:answer(Agent),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaevent">>, <<"event">> => <<"caller_offhold">> }),
	agent:rpc_call(Agent, <<"hangup">>, [UUID]),
	{match, _, #{ <<"statedata">> := #{ <<"time_limit">> := 1000 }}}
		= agent:wait_ws(Agent, #{ <<"command">> => <<"setchannel">>,<<"state">> => <<"wrapup">> }),
	agent:rpc_call(Agent, <<"end_wrapup">>, [UUID]),
	agent:wait_ws(Agent, #{ <<"command">> => <<"endchannel">> }).