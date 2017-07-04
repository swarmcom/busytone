-module(call).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([start_link/1, online/0, pid/1, vars/1, variables/1, hangup/1, answer/1, park/1, transfer/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	uuid,
	vars,
	variables
}).

start_link(UUID) ->
	gen_server:start_link(?MODULE, [UUID], []).

pid(UUID) ->
	gproc:whereis_name({n, l, {?MODULE, UUID}}).

safe_call(Id, Msg) when is_pid(Id) -> gen_server:call(Id, Msg);
safe_call(Id, Msg) ->
	case pid(Id) of
		undefined -> {error, no_pid};
		Pid -> gen_server:call(Pid, Msg)
	end.

safe_cast(Id, Msg) when is_pid(Id) -> gen_server:cast(Id, Msg);
safe_cast(Id, Msg) ->
	case pid(Id) of
		undefined -> {error, no_pid};
		Pid -> gen_server:cast(Pid, Msg)
	end.

vars(Id) -> safe_call(Id, vars).
variables(Id) -> safe_call(Id, variables).
hangup(Id) -> safe_cast(Id, hangup).
answer(Id) -> safe_cast(Id, answer).
park(Id) -> safe_cast(Id, park).
transfer(Id, Target) -> safe_cast(Id, {transfer, Target}).

sync_state(Pid) when is_pid(Pid) -> Pid ! sync_state.

online() ->
	Q = qlc:q([ UUID || {{n,l,{?MODULE, UUID}}, _Pid, _} <- gproc:table({l, n}) ]),
	qlc:e(Q).

init([UUID]) ->
	lager:notice("start, uuid:~s", [UUID]),
	gproc:reg({n, l, {?MODULE, UUID}}),
	sync_state(self()),
	{ok, #state{uuid = UUID}}.

handle_cast(answer, S=#state{uuid=UUID}) ->
	fswitch:api("uuid_answer ~s", [UUID]),
	{noreply, S};

handle_cast(hangup, S=#state{uuid=UUID}) ->
	fswitch:api("uuid_kill ~s", [UUID]),
	{noreply, S};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({call_event, {event,[UUID|Pairs]}}, S=#state{uuid=UUID}) ->
	{Vars, Variables} = fswitch:parse_uuid_dump(Pairs),
	handle_event(Vars, Variables, S#state{});

handle_info({call, {event,[UUID|Pairs]}}, S=#state{uuid=UUID}) ->
	{Vars, Variables} = fswitch:parse_uuid_dump(Pairs),
	handle_event(Vars, Variables, S#state{});

handle_info({call_hangup, UUID}, S=#state{uuid=UUID}) -> {stop, normal, S};

handle_info(sync_state, S=#state{uuid=UUID}) ->
	{ok, Dump} = fswitch:api("uuid_dump ~s", [UUID]),
	Pairs = fswitch:parse_uuid_dump_string(Dump),
	{Vars, Variables} = fswitch:parse_uuid_dump(Pairs),
	{noreply, S#state{vars = Vars, variables=Variables }};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(vars, _From, S=#state{vars = Vars}) -> {reply, Vars, S};
handle_call(variables, _From, S=#state{variables = Variables}) -> {reply, Variables, S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{uuid=UUID}) ->
	lager:notice("terminate, uuid:~s reason:~p", [UUID, _Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_event(Vars = #{ "Event-Name" := Ev }, _Variables, S=#state{}) when _Variables =:= #{} ->
	lager:info("ev:~p", [Ev]),
	{noreply, S#state{vars=Vars}};
handle_event(Vars = #{ "Event-Name" := Ev }, Variables, S=#state{}) ->
	lager:info("ev with vars:~p", [Ev]),
	{noreply, S#state{vars=Vars, variables=Variables}}.
