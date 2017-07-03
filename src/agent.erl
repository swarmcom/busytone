-module(agent).
-behaviour(gen_server).

-export([start_link/1, get/2, auth/3, rpc/3, available/1, release/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		number,
		reach,
		cookie,
		msg_id = 1
	}).

get(Pid, Uri) -> gen_server:call(Pid, {get, Uri}).
auth(Pid, Login, Password) -> gen_server:call(Pid, {auth, erlang:list_to_binary(Login), erlang:list_to_binary(Password)}).
rpc(Pid, Cmd, Args) -> gen_server:cast(Pid, {rpc, Cmd, Args}).

available(Pid) -> rpc(Pid, go_available, []).
release(Pid) -> rpc(Pid, go_released, []).

start_link(Number) ->
	gen_server:start_link(?MODULE, [Number], []).

init([Number]) ->
	{ok, Host} = application:get_env(busytone, reach_host),
	{ok, Port} = application:get_env(busytone, reach_port),
	{ok, Pid} = gun:open(Host, Port),
	monitor(process, Pid),
	{ok, #state{ reach = Pid, number = Number }}.

handle_info({gun_up, _Pid, http}, S) -> {noreply, S};
handle_info({gun_down, _Pid, http, closed, _, _}, S) -> {noreply, S};

handle_info({gun_response, _Pid, _StreamRef, nofin, _Status, Headers}, S) ->
	handle_cookie(to_map(Headers), S);
handle_info({gun_ws_upgrade, _Pid, ok, _Headers}, S) ->
	erlang:send_after(1000, self(), ping),
	{noreply, S};
handle_info({gun_ws, _Pid, {text, Text}}, S) ->
	Data = jiffy:decode(Text, [return_maps]),
	lager:info("in:~p", [Data]),
	{noreply, S};
handle_info({gun_data, _Pid, _StreamRef, fin, _Data}, S) ->
	{noreply, S};
handle_info(ping, S=#state{ reach = Pid, msg_id = Id }) ->
	gun:ws_send(Pid, {text, jiffy:encode(#{ id => Id, method => ping, params => [], jsonrpc => <<"2.0">> })}),
	erlang:send_after(10*1000, self(), ping),
	{noreply, S#state{msg_id = Id + 1}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({auth, Login, Password}, _From, S=#state{ reach = Pid }) ->
	gun:post(Pid, "/login", [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
		<<"username=", Login/binary, "&password=", Password/binary, "&remember=on">>),
	{reply, ok, S};
handle_call({get, Uri}, _From, S=#state{ reach = Pid }) ->
	gun:get(Pid, Uri),
	{reply, ok, S};
handle_call(_Msg, _From, S=#state{}) -> {reply, ok, S}.

handle_cast({rpc, Cmd, Args}, S=#state{ reach = Pid, msg_id = Id }) ->
	gun:ws_send(Pid, {text, jiffy:encode(#{ id => Id, method => Cmd, params => Args, jsonrpc => <<"2.0">> })}),
	{noreply, S#state{msg_id = Id + 1}};


handle_cast(_Msg, S=#state{}) -> {noreply, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

handle_cookie(#{ <<"set-cookie">> := <<"OUCX=",Cookie/binary>>}, S=#state{ reach = Pid }) ->
	Cookie1 = binary:replace(Cookie, <<"; Version=1; Path=/">>, <<>>),
	gun:ws_upgrade(Pid, <<"/wsock?token=", Cookie1/binary>>, []),
	{noreply, S#state{ cookie = Cookie1 }};
handle_cookie(_, S) -> {noreply, S}.

to_map(H) ->
	lists:foldl(fun({K,V}, M) -> M#{ K => V } end, #{}, H).

% connect and authenticate with reach
% accept and manipulate incoming calls