-module(call).
-behaviour(gen_server).

-export([
	start_link/1, stop/1, pid/1, tuple/1, alive/1, link_process/2, wait_hangup/1, subscribe/1,
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
	variables,
	wait_hangup = []
}).

start_link(UUID) ->
	gen_server:start_link(?MODULE, [UUID], []).

% gproc api
tuple(UUID) -> {?MODULE, UUID}.
pid({?MODULE, UUID}) -> pid(UUID);
pid(UUID) -> gproc:whereis_name({n, l, {?MODULE, UUID}}).
subscribe(UUID) -> gproc:reg({p, l, {?MODULE, UUID}}, subscribe).

vars(Id) -> gen_safe:call(Id, fun pid/1, vars).
variables(Id) -> gen_safe:call(Id, fun pid/1, variables).

hangup(Id) -> gen_safe:cast(Id, fun pid/1, hangup).
answer(Id) -> gen_safe:cast(Id, fun pid/1, answer).
park(Id) -> gen_safe:cast(Id, fun pid/1, park).
break(Id) -> gen_safe:cast(Id, fun pid/1, break).
stop(Id) -> gen_safe:cast(Id, fun pid/1, stop).
alive(Id) -> gen_safe:cast(Id, fun pid/1, alive).

link_process(Id, Pid) -> gen_safe:cast(Id, fun pid/1, {link_process, Pid}).
command(Id, Command, Args) -> gen_safe:cast(Id, fun pid/1, {command, Command, Args}).

wait_hangup(Id) -> gen_safe:call(Id, fun pid/1, {wait_hangup}).
deflect(Id, Target) -> gen_safe:call(Id, fun pid/1, {deflect, Target}).
display(Id, Name, Number) -> gen_safe:call(Id, fun pid/1, {display, Name, Number}).
getvar(Id, Name) -> gen_safe:call(Id, fun pid/1, {getvar, Name}).
hold(Id) -> gen_safe:call(Id, fun pid/1, {hold}).
hold(Id, off) -> gen_safe:call(Id, fun pid/1, {hold, off});
hold(Id, toggle) -> gen_safe:call(Id, fun pid/1, {hold, toggle}).
send_dtmf(Id, DTMF) -> gen_safe:call(Id, fun pid/1, {send_dtmf, DTMF}).
setvar(Id, Name) -> gen_safe:call(Id, fun pid/1, {setvar, Name}).
setvar(Id, Name, Value) -> gen_safe:call(Id, fun pid/1, {setvar, Name, Value}).
transfer(Id, Target) -> gen_safe:call(Id, fun pid/1, {transfer, Target}).
transfer(Id, Target, Dialplan) -> gen_safe:call(Id, fun pid/1, {transfer, Target, Dialplan}).
transfer(Id, Target, Dialplan, Context) -> gen_safe:call(Id, fun pid/1, {transfer, Target, Dialplan, Context}).
record(Id, Action=start, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path});
record(Id, Action=stop, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path});
record(Id, Action=mask, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path});
record(Id, Action=unmask, Path) -> gen_safe:call(Id, fun pid/1, {record, Action, Path}).

sync_state(Pid) when is_pid(Pid) -> Pid ! sync_state.

init([UUID]) ->
	lager:info("start, uuid:~s", [UUID]),
	gproc:reg({n, l, {?MODULE, UUID}}),
	sync_state(self()),
	{ok, #state{uuid = UUID}}.

handle_cast(answer, S=#state{uuid=UUID}) -> fswitch:api("uuid_answer ~s", [UUID]), {noreply, S};
handle_cast(hangup, S=#state{uuid=UUID}) -> fswitch:api("uuid_kill ~s", [UUID]), {noreply, S};
handle_cast(park, S=#state{uuid=UUID}) -> fswitch:api("uuid_park ~s", [UUID]), {noreply, S};
handle_cast(break, S=#state{uuid=UUID}) -> fswitch:api("uuid_break ~s", [UUID]), {noreply, S};
handle_cast({link_process, Pid}, S=#state{}) ->
	erlang:monitor(process, Pid),
	{noreply, S};
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
	bind_agent(UUID, Vars),
	handle_event(Vars, Variables, S);

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, S=#state{}) ->
	lager:info("owner is dead, pid:~p reason:~p", [_Pid, _Reason]),
	{stop, normal, S};

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
% just wait process to die
handle_call({wait_hangup}, From, S=#state{wait_hangup=WaitList}) -> {noreply, S#state{wait_hangup=[From|WaitList]}};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{uuid=UUID, wait_hangup=WaitList}) ->
	lager:info("terminate, uuid:~s reason:~p", [UUID, _Reason]),
	fswitch:api("uuid_kill ~s", [UUID]),
	[ gen_server:reply(Caller, ok) || Caller <- WaitList ],
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_event(Vars = #{ "Event-Name" := Ev }, Variables, S=#state{uuid=UUID}) ->
	lager:debug("ev:~p", [Ev]),
	gproc:send({p, l, {call, UUID}}, Ev),
	gproc:set_value({n, l, {?MODULE, UUID}}, Vars),
	{noreply, set_call_state(maybe_set_vairables(Variables, S#state{vars=Vars}))}.

set_call_state(S=#state{ vars = #{ "Channel-Call-State" := State } }) -> S#state{ call_state = State };
set_call_state(S) -> S.

maybe_set_vairables(Variables, S) when Variables =:= #{} -> S;
maybe_set_vairables(Variables, S) -> S#state{variables=Variables}.

bind_agent(UUID, #{ "Caller-Destination-Number" := Number, "Caller-Logical-Direction" := "inbound" }) ->
	[ erlang:monitor(process, agent:on_incoming(Agent, UUID)) || Agent <- agent_sup:by_number(Number) ];
bind_agent(_, _) -> ignore.
