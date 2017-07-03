-module(call).
-behaviour(gen_server).

-export([start_link/1, pid/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	uuid
}).

start_link(UUID) ->
	gen_server:start_link(?MODULE, [UUID], []).

pid(UUID) ->
	gproc:whereis_name({n, l, {?MODULE, UUID}}).

init([UUID]) ->
	lager:notice("start, uuid:~s", [UUID]),
	gproc:reg({n, l, {?MODULE, UUID}}),
	{ok, #state{uuid=UUID}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({call_event, {event,[UUID|Pairs]}}, S=#state{uuid=UUID}) ->
	handle_event(to_map(Pairs)),
	{noreply, S};

handle_info({call, {event,[UUID|Pairs]}}, S=#state{uuid=UUID}) ->
	handle_event(to_map(Pairs)),
	{noreply, S};

handle_info({call_hangup, UUID}, S=#state{uuid=UUID}) ->
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_event(_Event = #{ "Event-Name" := Ev }) ->
	lager:info("ev:~p", [Ev]),
	ok.

to_map(H) -> lists:foldl(fun({K,V}, M) -> M#{ K => V } end, #{}, H).
