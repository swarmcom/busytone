-module(t_core_inqueue_seq).
-export([main/0]).

% check agents are sorted by longest available state

main() ->
	{ok, A} = call_sup:originate(<<"default_queue">>),
	{ok, B} = call_sup:originate(<<"default_queue">>),
	{ok, C} = call_sup:originate(<<"default_queue">>),

	[#{ <<"id">> := A }, #{ <<"id">> := B }, #{ <<"id">> := C } ] = admin:call(inqueues, []).
