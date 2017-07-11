-module(call).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").

-export([
	start_link/1, pid/1, tuple/1, alive/1, link_process/2, wait_hangup/1, subscribe/2, unsubscribe/2,
	vars/1, variables/1,
	hangup/1, answer/1, park/1, break/1,
	deflect/2, display/3, getvar/2, hold/1, hold/2, setvar/2, setvar/3, send_dtmf/2, broadcast/2, displace/3,
	execute/3, tone_detect/4, detect_tone/2, stop_detect_tone/1,
	transfer/2, transfer/3, transfer/4, record/3, command/3,
	wait_event/2, wait_event/3,
	active/0, match_for/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	call_state,
	uuid,
	vars,
	variables,
	wait_hangup = [],
	event_log
}).

active() ->
	Q = qlc:q([ UUID || {{n,l,{?MODULE,UUID}}, _Pid, _} <- gproc:table({l, n}) ]),
	qlc:e(Q).

match_for(Map) when is_map(Map) ->
	Q = qlc:q([ UUID || {{n,l,{?MODULE,UUID}}, _Pid, M} <- gproc:table({l, n}), util:match_maps(Map, M) ]),
	qlc:e(Q);
match_for(Key) ->
	Q = qlc:q([ {UUID, maps:get(Key, M)} || {{n,l,{?MODULE,UUID}}, _Pid, M=#{}} <- gproc:table({l, n}), maps:is_key(Key, M) ]),
	qlc:e(Q).

start_link(UUID) ->
	gen_server:start_link(?MODULE, [UUID], []).

% gproc api
tuple(UUID) -> {?MODULE, UUID}.
pid({?MODULE, UUID}) -> pid(UUID);
pid(UUID) -> gproc:whereis_name({n, l, {?MODULE, UUID}}).
subscribe(uuid, UUID) -> gproc:reg({p, l, {?MODULE, uuid, UUID}}, subscribe);
subscribe(event, Event) -> gproc:reg({p, l, {?MODULE, event, Event}}, subscribe).
unsubscribe(uuid, UUID) -> gproc:unreg({p, l, {?MODULE, uuid, UUID}});
unsubscribe(event, Event) -> gproc:unreg({p, l, {?MODULE, event, Event}}).

vars(Id) -> gen_safe:call(Id, fun pid/1, vars).
variables(Id) -> gen_safe:call(Id, fun pid/1, variables).

hangup(Id) -> gen_safe:cast(Id, fun pid/1, hangup).
answer(Id) -> gen_safe:cast(Id, fun pid/1, answer).
park(Id) -> gen_safe:cast(Id, fun pid/1, park).
break(Id) -> gen_safe:cast(Id, fun pid/1, break).
alive(Id) -> gen_safe:cast(Id, fun pid/1, alive).
tone_detect(Id, EvName, Tone, Timeout) -> gen_safe:cast(Id, fun pid/1, {tone_detect, EvName, Tone, Timeout}).
detect_tone(Id, ToneName) -> gen_safe:cast(Id, fun pid/1, {detect_tone, ToneName}).
stop_detect_tone(Id) -> gen_safe:cast(Id, fun pid/1, stop_detect_tone).
broadcast(Id, Path) -> gen_safe:cast(Id, fun pid/1, {broadcast, Path}).
displace(Id, Cmd, Path) -> gen_safe:cast(Id, fun pid/1, {displace, Cmd, Path}).
execute(Id, Cmd, Args) -> gen_safe:cast(Id, fun pid/1, {execute, Cmd, Args}).

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

wait_event(Id, Match) -> wait_event(Id, Match, 5000).
wait_event(Id, Match, Timeout) ->
	gen_safe:call(Id, fun pid/1, {wait_event, Match}, Timeout).

sync_state(Pid) when is_pid(Pid) -> Pid ! sync_state.

init([UUID]) ->
	lager:info("start, uuid:~s", [UUID]),
	gproc:reg({n, l, {?MODULE, UUID}}),
	sync_state(self()),
	{ok, EvLog} = event_log:start_link(),
	{ok, #state{uuid = UUID, event_log = EvLog}}.

handle_cast(answer, S=#state{uuid=UUID}) -> fswitch:api("uuid_answer ~s", [UUID]), {noreply, S};
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
handle_cast(hangup, S=#state{uuid=UUID}) ->
	fswitch:api("uuid_kill ~s", [UUID]),
	{noreply, S};
% doesn't work?
handle_cast({tone_detect, Name, Tone, Timeout}, S=#state{uuid=UUID}) ->
	fswitch:api("tone_detect ~s ~s ~s r +~p stop_tone_detect '' 1", [UUID, Name, Tone, Timeout]),
	{noreply, S};
handle_cast({detect_tone, ToneName}, S=#state{uuid=UUID}) ->
	fswitch:api("spandsp_start_tone_detect ~s ~s", [UUID, ToneName]),
	{noreply, S};
handle_cast(stop_detect_tone, S=#state{uuid=UUID}) ->
	fswitch:api("spandsp_stop_tone_detect ~s", [UUID]),
	{noreply, S};

handle_cast({broadcast, Path}, S=#state{uuid=UUID}) ->
	fswitch:api("uuid_broadcast ~s ~s", [UUID, Path]),
	{noreply, S};
handle_cast({displace, Cmd, Path}, S=#state{uuid=UUID}) ->
	fswitch:api("uuid_displace ~s ~s ~s", [UUID, Cmd, Path]),
	{noreply, S};
handle_cast({execute, Cmd, Path}, S=#state{uuid=UUID}) ->
	fswitch:execute(UUID, Cmd, Path),
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
	handle_event(Vars#{ <<"Event-Name">> => <<"SYNC">> }, Variables, S);

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

handle_call({wait_event, Match}, From, S=#state{ event_log = EvLog }) ->
	case event_log:wait(EvLog, Match, From) of
		no_match -> {noreply, S};
		{match, _Ts, _} = Re -> {reply, Re, S}
	end;

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{uuid=UUID, wait_hangup=WaitList}) ->
	lager:info("terminate, uuid:~s reason:~p", [UUID, _Reason]),
	fswitch:api("uuid_kill ~s", [UUID]),
	[ gen_server:reply(Caller, ok) || Caller <- WaitList ],
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_event(Vars = #{ <<"Event-Name">> := Ev }, Variables, S=#state{uuid=UUID, event_log=EvLog}) ->
	lager:debug("ev:~s uuid:~s ~p", [Ev, UUID, maps:get(<<"Caller-Destination-Number">>, Vars, undefined)]),
	gproc:send({p, l, {?MODULE, uuid, UUID}}, {?MODULE, Ev}),
	gproc:send({p, l, {?MODULE, event, Ev}}, {?MODULE, UUID, Vars}),
	gproc:set_value({n, l, {?MODULE, UUID}}, Vars),
	case event_log:add(EvLog, Vars) of
		{match, Caller, {Ts, Msg}} -> gen_server:reply(Caller, {match, Ts, Msg});
		_ -> skip
	end,
	{noreply, set_call_state(maybe_set_vairables(Variables, S#state{vars=Vars}))}.

set_call_state(S=#state{ vars = #{ <<"Channel-Call-State">> := State } }) -> S#state{ call_state = State };
set_call_state(S) -> S.

maybe_set_vairables(Variables, S) when Variables =:= #{} -> S;
maybe_set_vairables(Variables, S) -> S#state{variables=Variables}.
