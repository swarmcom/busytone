-module(t_core_inqueue_seq).
-export([main/0]).

% check agents are sorted by longest available state

main() ->
	{ok, A} = call_sup:originate(<<"default_queue">>),
	{ok, B} = call_sup:originate(<<"default_queue">>),
	{ok, C} = call_sup:originate(<<"default_queue">>),

	[#{ <<"uuid">> := A }, #{ <<"uuid">> := B }, #{ <<"uuid">> := C } ] = admin:call(inqueues, []).
