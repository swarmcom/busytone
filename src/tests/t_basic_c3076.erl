-module(t_basic_c3076).
-export([main/0]).

% C3076: Wrap-up disabled

main() ->
	Queue = admin:new_queue(#{
		skills => #{ english => true },
		recipe => [],
		wrapup_enabled => false
	}),

	Agent = test_lib:available(admin:new_agent( #{ skills => #{ english => true } })),
	{ok, _} = call_sup:originate(Queue),
	test_lib:answer(Agent, <<"ch1">>),
	agent:wait_ws(Agent, #{ <<"command">> => <<"mediaevent">>, <<"event">> => <<"caller_offhold">> }),
	agent:rpc_call(Agent, <<"hangup">>, [<<"ch1">>]),
	agent:wait_ws(Agent, #{ <<"command">> => <<"endchannel">> }).