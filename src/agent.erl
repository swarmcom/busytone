-module(agent).
-behaviour(gen_server).
-include_lib("busytone/include/busytone.hrl").

-export([start_link/3, start/3, start/4, rpc/3, available/1, release/1, stop/1, calls/1, wait_ws/4, wait_ws/3, is_in/2, pid/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	agent,
	reach,
	cookie,
	ws_msg_id = 1,
	request,
	caller_pid,
	ws_log
}).

-define(STEP, 1000).

pid(Login) when is_binary(Login) -> pid(erlang:binary_to_list(Login));
pid(Login) -> gproc:whereis_name({n, l, {?MODULE, Login}}).

rpc(Id, Cmd, Args) -> gen_safe:cast(Id, fun pid/1, {rpc, Cmd, Args}).
calls(Id) -> gen_safe:call(Id, fun pid/1, calls).
stop(Id) -> gen_safe:cast(Id, fun pid/1, stop).
available(Id) -> rpc(Id, go_available, []).
release(Id) -> rpc(Id, go_released, []).

start_link(Host, Port, A=#agent{}) -> gen_server:start_link(?MODULE, [Host, Port, A], []).
start(Host, Port, A=#agent{}) -> gen_server:start(?MODULE, [Host, Port, A], []).
start(Pid, Host, Port, A=#agent{}) -> gen_server:start(?MODULE, [Pid, Host, Port, A], []).

% use call timeout as a failsafe
wait_ws(Id, Match, Error) -> wait_ws(Id, Match, 5000, Error).
wait_ws(Id, Match, Timeout, Error) ->
	gen_safe:call(Id, fun pid/1, {wait_ws, Match, Timeout, Error}, Timeout+?STEP).

init([Pid, Host, Port, A=#agent{}]) ->
	process_flag(trap_exit, true),
	link(Pid),
	{ok, State} = init([Host, Port, A]),
	{ok, State#state{caller_pid=Pid}};
init([Host, Port, A=#agent{login=Login}]) ->
	lager:notice("start agent, login:~p", [Login]),
	{ok, Reach} = gun:open(Host, Port),
	monitor(process, Reach),
	gproc:reg({n, l, {?MODULE, Login}}, A),
	{ok, #state{ reach = Reach, agent = A, ws_log = [] }}.

handle_info({gun_up, _Pid, http}, S) -> 
	self() ! auth,
	{noreply, S};
handle_info({gun_down, _Pid, http, closed, _, _}, S) -> {noreply, S};

handle_info({gun_response, _Pid, _StreamRef, nofin, 200, Headers}, S=#state{request=auth}) ->
	handle_cookie(to_map(Headers), S);
handle_info({gun_response, _Pid, _StreamRef, nofin, 401, _Headers}, S=#state{request=auth}) ->
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
		<<"username=", (to_bin(Login))/binary, "&password=", (to_bin(Password))/binary, "&remember=on">>),
	{noreply, S#state{request=auth}};

handle_info({wait_ws, _From, _Match, Timeout, Error}, S=#state{}) when Timeout =:= 0; Timeout < 0 -> {stop, Error, S};
handle_info({wait_ws, From, Match, Timeout, Error}, S=#state{ws_log=Log}) ->
	case search_ws_log(Match, Log) of
		true -> gen_server:reply(From, Match);
		false -> timer:send_after(?STEP, {wait_ws, From, Match, Timeout-?STEP, Error})
	end,
	{noreply, S};

handle_info({'EXIT', Pid, _}, S=#state{caller_pid=Pid}) ->
	{stop, normal, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(calls, _From, S=#state{agent=#agent{number=Number}}) ->
	Match = call_manager:agent_match(Number),
	Re = call_manager:match_for(Match),
	{reply, Re, S};

handle_call({wait_ws, Match, Timeout, Error}, From, S=#state{}) ->
	self() ! {wait_ws, From, Match, Timeout, Error},
	{noreply, S};

handle_call(_Msg, _From, S=#state{}) ->
	{reply, ok, S}.

handle_cast({rpc, Cmd, Args}, S=#state{ reach = Pid, ws_msg_id = Id }) ->
	gun:ws_send(Pid, {text, jiffy:encode(#{ id => Id, method => Cmd, params => Args, jsonrpc => <<"2.0">> })}),
	{noreply, S#state{ws_msg_id = Id + 1}};
handle_cast(stop, S=#state{}) ->
	{stop, normal, S};

handle_cast(_Msg, S=#state{}) -> {noreply, S}.
terminate(_Reason, _S=#state{reach=Pid}) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	gun:close(Pid),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_cookie(#{ <<"set-cookie">> := <<"OUCX=",Cookie/binary>> }, S=#state{ reach = Pid }) ->
	Cookie1 = binary:replace(Cookie, <<"; Version=1; Path=/">>, <<>>),
	gun:ws_upgrade(Pid, <<"/wsock?token=", Cookie1/binary>>, []),
	{noreply, S#state{ cookie = Cookie1 }};
handle_cookie(_, S) -> {noreply, S}.

handle_ws_text(#{ <<"result">> := #{ <<"pong">> := _ } }, S) -> {noreply, S};
handle_ws_text(Text, S=#state{ ws_log = Log }) ->
	lager:info("ws in:~p", [Text]),
	{noreply, S#state{ ws_log = [Text | Log ]}}.

search_ws_log(_, []) -> false;
search_ws_log(Match, [Msg|Log]) ->
	case is_in(Match, Msg) of
		true -> true;
		false -> search_ws_log(Match, Log)
	end.

is_in(Inner, Outer) ->
	L = maps:to_list(Inner) -- maps:to_list(Outer),
	case erlang:length(L) of
		0 -> true;
		_N -> false
	end.

to_map(H) ->
	lists:foldl(fun({K,V}, M) -> M#{ K => V } end, #{}, H).

to_bin(L) when is_list(L) -> erlang:list_to_binary(L).

% connect and authenticate with reach
% accept and manipulate incoming calls