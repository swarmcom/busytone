-module(agent).
-behaviour(gen_server).
-include_lib("busytone/include/busytone.hrl").
-include_lib("fswitch/include/fswitch.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	start_link/3, start/3, start/4, pid/1,
	rpc/4, call/4, available/1, release/1, stop/1,
	calls/1, wait_for_login/1, wait_for_call/1, on_incoming/2,
	wait_ws/4, wait_ws/3, wait_ws/2, wait_ev/3,
	online/0, by_number/1, ws_debug_filter/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	agent,
	reach,
	cookie,
	ws_msg_id = 1,
	http_request,
	caller_pid,
	ws_log,
	wait_for_incoming,
	wait_for_login,
	incoming_call,
	ws_debug_filter = []
}).

-define(STEP, 1000).

pid(Login) -> gproc:whereis_name({n, l, {?MODULE, Login}}).

call(Agent, Module, Cmd, Args) ->
	MsgId = rpc(Agent, Module, Cmd, Args),
	{match, _, #{ <<"reply">> := Re } } = wait_ws(Agent, #{ <<"id">> => MsgId }),
	Re.

login(Login, Password) ->
	try
		M = #{ <<"id">> := _AgentId } = call(Login, ws_agent, auth, [Login, Password, false]),
		update(Login, M)
	catch _:Err ->
		lager:error("authenticate:~p", [Err]),
		agent:stop(Login)
	end.

online() ->
	Q = qlc:q([ A || {_, _Pid, A=#agent{}} <- gproc:table({l, n}) ]),
	qlc:e(Q).

by_number(Number) ->
	Q = qlc:q([ Login || {_, _Pid, #agent{login=Login, number=N}} <- gproc:table({l, n}), N =:= Number ]),
	qlc:e(Q).

rpc(Id, F, A) -> rpc(Id, ws_agent, F, A).
rpc(Id, M, F, A) -> gen_safe:call(Id, fun pid/1, {rpc, M, F, A}).
calls(Id) -> gen_safe:call(Id, fun pid/1, calls).
ws_debug_filter(Id, Filter) -> gen_safe:call(Id, fun pid/1, {ws_debug_filter, Filter}).
wait_for_call(Id) -> gen_safe:call(Id, fun pid/1, wait_for_call).
wait_for_login(Id) -> gen_safe:call(Id, fun pid/1, wait_for_login).
on_incoming(Id, UUID) -> gen_safe:call(Id, fun pid/1, {on_incoming, UUID}).
stop(Id) -> gen_safe:call(Id, fun pid/1, stop).
available(Id) -> rpc(Id, available, []).
release(Id) -> rpc(Id, release, []).
update(Id, Info) -> gen_safe:call(Id, fun pid/1, {update, Info}).

start_link(Host, Port, A=#agent{}) -> gen_server:start_link(?MODULE, [Host, Port, A], []).
start(Host, Port, A=#agent{}) -> gen_server:start(?MODULE, [Host, Port, A], []).
start(Pid, Host, Port, A=#agent{}) -> gen_server:start(?MODULE, [Pid, Host, Port, A], []).

% use call timeout as a failsafe
wait_ws(Id, Ev) when is_binary(Ev) -> wait_ws(Id, #{ <<"event">> => Ev });
wait_ws(Id, Match) -> wait_ws(Id, Match, 10).
wait_ws(Id, Match, Depth) -> wait_ws(Id, Match, Depth, 5000).
wait_ws(Id, Match, Depth, Timeout) ->
	gen_safe:call(Id, fun pid/1, {wait_ws, Match, Depth}, Timeout).

wait_ev(Id, UUID, Ev) -> wait_ws(Id, #{<<"call_event">> => Ev, <<"uuid">> => UUID }, 100).

% this clause is to link with caller process of fun call_sup:originate
init([Pid, Host, Port, A=#agent{}]) ->
	process_flag(trap_exit, true),
	link(Pid),
	{ok, State} = init([Host, Port, A]),
	{ok, State#state{caller_pid=Pid}};
init([Host, Port, A=#agent{login=Login}]) ->
	lager:info("~p start", [Login]),
	{ok, Reach} = gun:open(Host, Port),
	monitor(process, Reach),
	gproc:reg({n, l, {?MODULE, Login}}, A),
	call:subscribe(event, <<"SYNC">>),
	{ok, WsLog} = event_log:start_link(),
	{ok, #state{ reach = Reach, agent = A, ws_log = WsLog }}.

handle_info({gun_up, Pid, http}, S) ->
	gun:ws_upgrade(Pid, <<"/ws">>, []),
	{noreply, S};
handle_info({gun_down, _Pid, http, closed, _, _}, S) -> {noreply, S};
handle_info({gun_down, _Pid, ws, closed, _, _}, S) -> {stop, normal, S};

handle_info({gun_ws_upgrade, _Pid, ok, _Headers}, S=#state{agent=#agent{login=Login, password=Password}}) ->
	erlang:send_after(30000, self(), ping),
	spawn(fun() -> login(Login, Password) end),
	{noreply, S#state{}};
handle_info({gun_ws, _Pid, {text, Text}}, S) ->
	lager:debug("ws in:~s", [Text]),
	handle_ws_text(jiffy:decode(Text, [return_maps]), S);
handle_info({gun_data, _Pid, _StreamRef, fin, _Data}, S) ->
	{noreply, S};
handle_info({gun_data, _Pid, _StreamRef, nofin, _Data}, S) ->
	{noreply, S};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{ incoming_call={Ref, _UUID} }) ->
	{noreply, S#state{incoming_call=undefined}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S=#state{agent=#agent{login=_Agent}, reach=Pid}) ->
	lager:info("~s reach connection is dead, pid:~p reason:~p", [_Agent, Pid, _Reason]),
	{stop, normal, S};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S=#state{agent=#agent{login=Agent}}) ->
	lager:debug("~s incoming call pid:~p is down", [Agent, Pid]),
	{noreply, S};

handle_info(#call_event{uuid=UUID, vars=#{
	<<"Caller-Destination-Number">> := Number, <<"Caller-Logical-Direction">> := <<"inbound">> }}, S=#state{ agent=#agent{ number = Number }}) ->
	handle_incoming_call(UUID, S);
handle_info(#call_event{}, S=#state{}) -> {noreply, S};

handle_info({gun_ws, _Pid, {close, _, _}}, S) ->
	{stop, normal, S};

handle_info({'EXIT', Pid, _}, S=#state{caller_pid=Pid}) ->
	{stop, normal, S};

handle_info(ping, S=#state{reach=Pid}) ->
	erlang:send_after(30000, self(), ping),
	gun:ws_send(Pid, ping),
	{noreply, S};

handle_info(_Info, S=#state{agent=#agent{login=_Login}}) ->
	lager:error("~s unhandled info:~p", [_Login, _Info]),
	{noreply, S}.

handle_call(calls, _From, S=#state{incoming_call=undefined}) -> {reply, [], S};
handle_call(calls, _From, S=#state{incoming_call={_Ref, UUID}}) -> {reply, [UUID], S};

handle_call(wait_for_call, From, S=#state{incoming_call=undefined}) ->
	{noreply, S#state{ wait_for_incoming=From }};
handle_call(wait_for_call, _From, S=#state{incoming_call={_Ref, UUID}}) ->
	{reply, [UUID], S#state{wait_for_incoming=undefined, incoming_call=undefined}};

handle_call(wait_for_login, From, S=#state{agent=#agent{agent_id=undefined}}) ->
	{noreply, S#state{ wait_for_login=From }};
handle_call(wait_for_login, _From, S=#state{agent=#agent{agent_id=AgentId}}) ->
	{reply, AgentId, S#state{}};

handle_call({wait_ws, Match, Depth}, From, S=#state{ ws_log = WsLog }) ->
	case event_log:wait(WsLog, Match, From, Depth) of
		no_match -> {noreply, S};
		{match, _Ts, _Msg} = Re -> {reply, Re, S}
	end;
handle_call(stop, _, S=#state{}) ->
	{stop, normal, ok, S};

handle_call({rpc, M, F, A}, _, S=#state{}) ->
	Id = msg(M, F, A, S),
	{reply, Id, S#state{ws_msg_id = Id + 1}};

handle_call({ws_debug_filter, Filter}, _From, S=#state{}) ->
	{reply, ok, S#state{ ws_debug_filter=Filter }};

handle_call({update, #{ <<"id">> := AgentId, <<"uri">> := Uri }}, _From, S=#state{agent=#agent{}=A}) ->
	gproc:reg({n, l, {?MODULE, AgentId}}, A),
	{reply, ok, maybe_notify_caller(S#state{agent=A#agent{agent_id=AgentId, number=Uri}})};

handle_call(_Msg, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Msg]),
	{reply, ok, S}.

handle_cast(_Msg, S=#state{}) -> {noreply, S}.

terminate(_Reason, _S=#state{reach=Pid, agent=#agent{login=Login}}) ->
	lager:info("~s terminate, reason:~p", [Login, _Reason]),
	gun:close(Pid),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_ws_text(Msg, S=#state{ agent=Agent, ws_log=WsLog, ws_debug_filter=Filter }) ->
	maybe_debug(Agent, Msg, Filter),
	case event_log:add(WsLog, Msg) of
		{match, Caller, {Ts, Msg}} -> gen_server:reply(Caller, {match, Ts, Msg});
		_ -> skip
	end,
	{noreply, S}.

handle_incoming_call(UUID, S=#state{}) ->
	Ref = erlang:monitor(process, call:link_process(UUID, self())),
	call:subscribe(uuid, UUID),
	{noreply, maybe_notify_waiter(S#state{incoming_call={Ref, UUID}})}.

maybe_notify_caller(S=#state{wait_for_login=undefined}) -> S;
maybe_notify_caller(S=#state{wait_for_login=Waiter, agent=#agent{agent_id=AgentId}}) ->
	gen_server:reply(Waiter, AgentId),
	S#state{wait_for_login=undefined}.

maybe_notify_waiter(S=#state{wait_for_incoming=undefined}) -> S;
maybe_notify_waiter(S=#state{wait_for_incoming=From, incoming_call={_Ref, UUID}}) ->
	gen_server:reply(From, [UUID]),
	S#state{wait_for_incoming=undefined, incoming_call=undefined}.

maybe_debug(_Agent, Msg = #{ <<"event">> := Event }, Filter) when is_list(Filter) ->
	maybe_debug(_Agent, Msg, Filter == [] orelse not lists:member(Event, Filter));
maybe_debug(_Agent, Msg, Filter) when is_list(Filter) -> maybe_debug(_Agent, Msg, true);
maybe_debug(_Agent, _Msg, false) -> skip;
maybe_debug(#agent{login=Agent}, Msg, true) -> lager:debug("~s ws in ~p", [Agent, Msg]).

msg(M, F, A, #state{reach=Pid, ws_msg_id=Id, agent=#agent{login=Login}}) ->
	Text = jiffy:encode(#{ id => Id, type => call, args => [M, F, A] }),
	lager:debug("~s ws out ~s", [Login, Text]),
	gun:ws_send(Pid, {text, Text}),
	Id.

% connect and authenticate with reach
% accept and manipulate incoming calls
