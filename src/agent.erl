-module(agent).
-behaviour(gen_server).
-include_lib("busytone/include/busytone.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	start_link/3, start/3, start/4, pid/1,
	rpc/3, rpc_call/3, available/1, release/1, stop/1,
	calls/1, wait_for_call/1, on_incoming/2,
	wait_ws/3, wait_ws/2,
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
	incoming_call,
	ws_debug_filter = [ <<"cpx_agent_change">>, <<"call_count_update">> ]
}).

-define(STEP, 1000).

pid(Login) -> gproc:whereis_name({n, l, {?MODULE, Login}}).

rpc_call(Agent, Cmd, Args) ->
	MsgId = rpc(Agent, Cmd, Args),
	{match, _, #{ <<"result">> := Re } } = agent:wait_ws(Agent, #{ <<"id">> => MsgId }),
	Re.

online() ->
	Q = qlc:q([ A || {_, _Pid, A=#agent{}} <- gproc:table({l, n}) ]),
	qlc:e(Q).

by_number(Number) ->
	Q = qlc:q([ Login || {_, _Pid, #agent{login=Login, number=N}} <- gproc:table({l, n}), N =:= Number ]),
	qlc:e(Q).

rpc(Id, Cmd, Args) -> gen_safe:call(Id, fun pid/1, {rpc, Cmd, Args}).
calls(Id) -> gen_safe:call(Id, fun pid/1, calls).
ws_debug_filter(Id, Filter) -> gen_safe:call(Id, fun pid/1, {ws_debug_filter, Filter}).
wait_for_call(Id) -> gen_safe:call(Id, fun pid/1, wait_for_call).
on_incoming(Id, UUID) -> gen_safe:call(Id, fun pid/1, {on_incoming, UUID}).
stop(Id) -> gen_safe:call(Id, fun pid/1, stop).
available(Id) -> rpc(Id, go_available, []).
release(Id) -> rpc(Id, go_released, []).

start_link(Host, Port, A=#agent{}) -> gen_server:start_link(?MODULE, [Host, Port, A], []).
start(Host, Port, A=#agent{}) -> gen_server:start(?MODULE, [Host, Port, A], []).
start(Pid, Host, Port, A=#agent{}) -> gen_server:start(?MODULE, [Pid, Host, Port, A], []).

% use call timeout as a failsafe
wait_ws(Id, Match) -> wait_ws(Id, Match, 5000).
wait_ws(Id, Match, Timeout) ->
	gen_safe:call(Id, fun pid/1, {wait_ws, Match}, Timeout).

% this clause is to link with caller process of fun call_sup:originate
init([Pid, Host, Port, A=#agent{}]) ->
	process_flag(trap_exit, true),
	link(Pid),
	{ok, State} = init([Host, Port, A]),
	{ok, State#state{caller_pid=Pid}};
init([Host, Port, A=#agent{login=Login}]) ->
	lager:info("start agent, login:~p", [Login]),
	{ok, Reach} = gun:open(Host, Port),
	monitor(process, Reach),
	gproc:reg({n, l, {?MODULE, Login}}, A),
	call:subscribe(event, <<"SYNC">>),
	{ok, WsLog} = event_log:start_link(),
	{ok, #state{ reach = Reach, agent = A, ws_log = WsLog }}.

handle_info({gun_up, _Pid, http}, S) -> 
	self() ! auth,
	{noreply, S};
handle_info({gun_down, _Pid, http, closed, _, _}, S) -> {noreply, S};
handle_info({gun_down, _Pid, ws, closed, _, _}, S) -> {stop, normal, S};

handle_info({gun_response, _Pid, _StreamRef, nofin, 200, Headers}, S=#state{http_request=auth}) ->
	handle_cookie(to_map(Headers), S);
handle_info({gun_response, _Pid, _StreamRef, nofin, 401, _Headers}, S=#state{http_request=auth}) ->
	{stop, auth_failure, S};
handle_info({gun_response, _Pid, _StreamRef, nofin, Status, Headers}, S) ->
	lager:info("status:~p headers:~p", [Status, Headers]),
	{noreply, S};
handle_info({gun_ws_upgrade, _Pid, ok, _Headers}, S) ->
	erlang:send_after(1000, self(), ping),
	{noreply, S};
handle_info({gun_ws, _Pid, {text, Text}}, S) ->
	handle_ws_text(jiffy:decode(Text, [return_maps]), S);
handle_info({gun_data, _Pid, _StreamRef, fin, _Data}, S) ->
	{noreply, S};
handle_info({gun_data, _Pid, _StreamRef, nofin, _Data}, S) ->
	{noreply, S};

handle_info(ping, S=#state{ reach = Pid, ws_msg_id = Id }) ->
	gun:ws_send(Pid, {text, jiffy:encode(#{ id => Id, method => ping, params => [], jsonrpc => <<"2.0">> })}),
	erlang:send_after(10*1000, self(), ping),
	{noreply, S#state{ws_msg_id = Id + 1}};

handle_info(auth, S=#state{ reach = Pid, agent = #agent{login=Login, password=Password} }) ->
	gun:post(Pid, "/login", [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
		<<"username=", Login/binary, "&password=", Password/binary, "&remember=on">>),
	{noreply, S#state{http_request=auth}};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{ incoming_call={Ref, _UUID} }) ->
	{noreply, S#state{incoming_call=undefined}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S=#state{ reach=Pid }) ->
	lager:info("reach connection is dead, pid:~p reason:~p", [Pid, _Reason]),
	{stop, normal, S};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S=#state{agent=#agent{login=Agent}}) ->
	lager:debug("incoming call is down, agent:~s pid:~p", [Agent, Pid]),
	{noreply, S};

handle_info({call, UUID, #{ <<"Caller-Destination-Number">> := Number, <<"Caller-Logical-Direction">> := <<"inbound">> }}, S=#state{ agent=#agent{ number = Number }}) ->
	handle_incoming_call(UUID, S);
handle_info({call, _, _}, S=#state{}) -> {noreply, S};

handle_info({call, _}, S=#state{}) -> {noreply, S};

handle_info({'EXIT', Pid, _}, S=#state{caller_pid=Pid}) ->
	{stop, normal, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(calls, _From, S=#state{incoming_call=undefined}) -> {reply, [], S};
handle_call(calls, _From, S=#state{incoming_call={_Ref, UUID}}) -> {reply, [UUID], S};

handle_call(wait_for_call, From, S=#state{incoming_call=undefined}) ->
	{noreply, S#state{ wait_for_incoming=From }};
handle_call(wait_for_call, _From, S=#state{incoming_call={_Ref, UUID}}) ->
	{reply, [UUID], S#state{wait_for_incoming=undefined, incoming_call=undefined}};

handle_call({wait_ws, Match}, From, S=#state{ ws_log = WsLog }) ->
	case event_log:wait(WsLog, Match, From, 10) of
		no_match -> {noreply, S};
		{match, _Ts, _Msg} = Re -> {reply, Re, S}
	end;
handle_call(stop, _, S=#state{}) ->
	{stop, normal, ok, S};

handle_call({rpc, Cmd, Args}, _, S=#state{ reach = Pid, ws_msg_id = Id }) ->
	Text = jiffy:encode(#{ id => Id, method => Cmd, params => Args, jsonrpc => <<"2.0">> }),
	lager:debug("ws out:~s", [Text]),
	gun:ws_send(Pid, {text, Text}),
	{reply, Id, S#state{ws_msg_id = Id + 1}};

handle_call({ws_debug_filter, Filter}, _From, S=#state{}) ->
	{reply, ok, S#state{ ws_debug_filter=Filter }};

handle_call(_Msg, _From, S=#state{}) ->
	{reply, ok, S}.

handle_cast(_Msg, S=#state{}) -> {noreply, S}.

terminate(_Reason, _S=#state{reach=Pid, agent=#agent{login=Login}}) ->
	lager:info("terminate, login:~s reason:~p", [Login, _Reason]),
	gun:close(Pid),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_cookie(#{ <<"set-cookie">> := <<"OUCX=",Cookie/binary>> }, S=#state{ reach = Pid }) ->
	Cookie1 = binary:replace(Cookie, <<"; Version=1; Path=/">>, <<>>),
	gun:ws_upgrade(Pid, <<"/wsock?token=", Cookie1/binary>>, []),
	{noreply, S#state{ cookie = Cookie1 }};
handle_cookie(_, S) -> {noreply, S}.

handle_ws_text(#{ <<"result">> := #{ <<"pong">> := _ } }, S) ->
	{noreply, S};
handle_ws_text(Msg, S=#state{ ws_log=WsLog, ws_debug_filter=Filter }) ->
	maybe_debug(Msg, Filter),
	case event_log:add(WsLog, Msg) of
		{match, Caller, {Ts, Msg}} -> gen_server:reply(Caller, {match, Ts, Msg});
		_ -> skip
	end,
	{noreply, S}.

handle_incoming_call(UUID, S=#state{}) ->
	Ref = erlang:monitor(process, call:link_process(UUID, self())),
	call:subscribe(uuid, UUID),
	{noreply, maybe_notify_waiter(S#state{incoming_call={Ref, UUID}})}.

maybe_notify_waiter(S=#state{wait_for_incoming=undefined}) -> S;
maybe_notify_waiter(S=#state{wait_for_incoming=From, incoming_call={_Ref, UUID}}) ->
	gen_server:reply(From, [UUID]),
	S#state{wait_for_incoming=undefined, incoming_call=undefined}.

to_map(H) ->
	lists:foldl(fun({K,V}, M) -> M#{ K => V } end, #{}, H).

maybe_debug(Msg = #{ <<"event">> := Event }, Filter) when is_list(Filter) ->
	maybe_debug(Msg, Filter == [] orelse not lists:member(Event, Filter));
maybe_debug(Msg, Filter) when is_list(Filter) -> maybe_debug(Msg, true);
maybe_debug(_Msg, false) -> skip;
maybe_debug(Msg, true) -> lager:debug("ws in, msg:~p", [Msg]).

% connect and authenticate with reach
% accept and manipulate incoming calls
