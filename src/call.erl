-module(call).
-behaviour(gen_server).

-export([
	start_link/1, stop/1, pid/1, tuple/1, alive/1,
	vars/1, variables/1,
	hangup/1, answer/1, park/1, break/1,
	deflect/2, display/3, getvar/2, hold/1, hold/2, setvar/2, setvar/3, send_dtmf/2,
	transfer/2, transfer/3, transfer/4, record/3, command/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	call_state,
	uuid,
	vars,
	variables
}).

start_link(UUID) ->
	gen_server:start_link(?MODULE, [UUID], []).

tuple(UUID) -> {?MODULE, UUID}.
pid({?MODULE, UUID}) -> pid(UUID);
pid(UUID) -> gproc:whereis_name({n, l, {?MODULE, UUID}}).

call(Id, Msg) when is_pid(Id) -> gen_server:call(Id, Msg);
call(Id, Msg) ->
	case pid(Id) of
		undefined -> {error, no_pid};
		Pid -> gen_server:call(Pid, Msg)
	end.

cast(Id, Msg) when is_pid(Id) -> gen_server:cast(Id, Msg);
cast(Id, Msg) ->
	case pid(Id) of
		undefined -> {error, no_pid};
		Pid -> gen_server:cast(Pid, Msg)
	end.

vars(Id) -> call(Id, vars).
variables(Id) -> call(Id, variables).

hangup(Id) -> cast(Id, hangup).
answer(Id) -> cast(Id, answer).
park(Id) -> cast(Id, park).
break(Id) -> cast(Id, break).
stop(Id) -> cast(Id, stop).
alive(Id) -> cast(Id, alive).
command(Id, Command, Args) -> cast(Id, {command, Command, Args}).

deflect(Id, Target) -> call(Id, {deflect, Target}).
display(Id, Name, Number) -> call(Id, {display, Name, Number}).
getvar(Id, Name) -> call(Id, {getvar, Name}).
hold(Id) -> call(Id, {hold}).
hold(Id, off) -> call(Id, {hold, off});
hold(Id, toggle) -> call(Id, {hold, toggle}).
send_dtmf(Id, DTMF) -> call(Id, {send_dtmf, DTMF}).
setvar(Id, Name) -> call(Id, {setvar, Name}).
setvar(Id, Name, Value) -> call(Id, {setvar, Name, Value}).
transfer(Id, Target) -> call(Id, {transfer, Target}).
transfer(Id, Target, Dialplan) -> call(Id, {transfer, Target, Dialplan}).
transfer(Id, Target, Dialplan, Context) -> call(Id, {transfer, Target, Dialplan, Context}).
record(Id, Action=start, Path) -> call(Id, {record, Action, Path});
record(Id, Action=stop, Path) -> call(Id, {record, Action, Path});
record(Id, Action=mask, Path) -> call(Id, {record, Action, Path});
record(Id, Action=unmask, Path) -> call(Id, {record, Action, Path}).

sync_state(Pid) when is_pid(Pid) -> Pid ! sync_state.

init([UUID]) ->
	lager:notice("start, uuid:~s", [UUID]),
	gproc:reg({n, l, {?MODULE, UUID}}),
	sync_state(self()),
	{ok, #state{uuid = UUID}}.

handle_cast(answer, S=#state{uuid=UUID}) -> fswitch:api("uuid_answer ~s", [UUID]), {noreply, S};
handle_cast(hangup, S=#state{uuid=UUID}) -> fswitch:api("uuid_kill ~s", [UUID]), {noreply, S};
handle_cast(park, S=#state{uuid=UUID}) -> fswitch:api("uuid_park ~s", [UUID]), {noreply, S};
handle_cast(break, S=#state{uuid=UUID}) -> fswitch:api("uuid_break ~s", [UUID]), {noreply, S};
handle_cast(alive, S=#state{uuid=UUID}) ->
	{ok, "true"} = fswitch:api("uuid_exists ~s", [UUID]),
	{noreply, S};
handle_cast({command, Command, Args}, S=#state{uuid=UUID}) ->
	fswitch:command(UUID, Command, Args),
	{noreply, S};
handle_cast(stop, S=#state{}) -> {stop, normal, S};

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
	handle_event(Vars, Variables, S);

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(vars, _From, S=#state{vars = Vars}) -> {reply, Vars, S};
handle_call(variables, _From, S=#state{variables = Variables}) -> {reply, Variables, S};

handle_call({deflect, Target}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_deflect ~s ~s", [UUID, Target]), S};
handle_call({display, Name, Number}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_display ~s ~s|~s", [UUID, Name, Number]), S};
handle_call({getvar, Name}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_getvar ~s ~s", [UUID, Name]), S};
handle_call({hold}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_hold ~s", [UUID]), S};
handle_call({hold, Cmd}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_hold ~s ~s", [Cmd, UUID]), S};
handle_call({send_dtmf, DTMF}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_send_dtmf ~s ~s", [UUID, DTMF]), S};
handle_call({setvar, Name, Value}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_setvar ~s ~s ~s", [UUID, Name, Value]), S};
handle_call({setvar, Name}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_setvar ~s ~s", [UUID, Name]), S};
handle_call({transfer, Target}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_transfer ~s ~s", [UUID, Target]), S};
handle_call({transfer, Target, Dialplan}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_transfer ~s ~s ~s", [UUID, Target, Dialplan]), S};
handle_call({transfer, Target, Dialplan, Context}, _From, S=#state{uuid=UUID}) ->
	{reply, fswitch:api("uuid_transfer ~s ~s ~s ~s", [UUID, Target, Dialplan, Context]), S};
handle_call({record, Action, Path}, _From, S=#state{uuid=UUID}) -> {reply, fswitch:api("uuid_record ~s ~s ~s", [UUID, Action, Path]), S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{uuid=UUID}) ->
	lager:notice("terminate, uuid:~s reason:~p", [UUID, _Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_event(Vars = #{ "Event-Name" := Ev }, _Variables, S=#state{uuid=UUID}) when _Variables =:= #{} ->
	lager:debug("ev:~p", [Ev]),
	gproc:set_value({n, l, {?MODULE, UUID}}, Vars),
	{noreply, set_call_state(S#state{vars=Vars})};
handle_event(Vars = #{ "Event-Name" := Ev }, Variables, S=#state{uuid=UUID}) ->
	lager:debug("ev with vars:~p", [Ev]),
	gproc:set_value({n, l, {?MODULE, UUID}}, Vars),
	{noreply, set_call_state(S#state{vars=Vars, variables=Variables})}.

set_call_state(S=#state{ vars = #{ "Channel-Call-State" := State } }) -> S#state{ call_state = State };
set_call_state(S) -> S.