-module(call_sup).
-behaviour(gen_server).

% accept and manage incoming agent callls
-define(ORIGINATE_TIMEOUT, 5000).

-export([
	start_link/0, originate/3, originate/2, originate/1,
	ensureTalking/2, ensureTalking/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	originate_map
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

originate(Url, Exten, Opts, Timeout) -> gen_server:call(?MODULE, {originate, self(), Timeout, Url, Exten, Opts}, Timeout).
originate(Url, Exten, Opts) -> originate(Url, Exten, Opts, ?ORIGINATE_TIMEOUT).
originate(Url, Opts) -> originate(Url, self_exten(), Opts).
originate(Url) -> originate(Url, []).

self_exten() -> io_lib:format("&erlang('~s:! ~s')", [?MODULE, node()]).

init([]) ->
	{ok, #state{originate_map=#{}}}.

handle_info({freeswitch_sendmsg, UUID}, #state{}=S) ->
	lager:info("incoming message, uuid:~s", [UUID]),
	{ok, _Pid} = call:start_link(UUID),
	{noreply, S};

handle_info({get_pid, UUID, Ref, From}, #state{originate_map=M}=S) ->
	lager:info("call control request, uuid:~s", [UUID]),
	CallPid =
		case call:pid(UUID) of
			undefined -> {ok, Pid} = call:start_link(UUID), Pid;
			Pid -> Pid
		end,
	From ! {Ref, CallPid}, % notify fs back
	case maps:is_key(UUID, M) of
		true ->
			{OwnerPid, Caller} = maps:get(UUID, M),
			call:link_process(UUID, OwnerPid),
			gen_server:reply(Caller, {ok, UUID}),
			{noreply, S#state{originate_map=maps:remove(UUID, M)}};
		false ->
			{noreply, S}
	end;

handle_info({originate_cleanup, UUID, _From}, S=#state{ originate_map=M }) ->
	case maps:is_key(UUID, M) of
		true -> {noreply, S#state{ originate_map=maps:remove(UUID, M) }};
		false -> {noreply, S}
	end;

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_call({originate, OwnerPid, Timeout, URL, Exten, Opts}, From, S=#state{originate_map=M}) ->
	case fswitch:api("originate ~s~s ~s", [stringify_opts(Opts), URL, Exten]) of
		{ok, "+OK "++UUID} ->
			UUID1 = trim(UUID),
			timer:send_after(Timeout+1000, {originate_cleanup, UUID1, From}), 
			{noreply, S#state{ originate_map = M#{ UUID1 => {OwnerPid, From} }}};
		_Err ->
			{reply, _Err, S}
	end;

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:info("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

trim(Str) -> string:left(Str, erlang:length(Str)-1).

stringify_opts([]) -> "";
stringify_opts(Opts) when is_list(Opts) ->
	Str = string:join([ io_lib:format("~s=~s", [K,V]) || {K, V} <- Opts ], ","),
	io_lib:format("{~s}", [Str]).

ensureTalking(UUID1, UUID2) -> ensureTalking(UUID1, UUID2, 5000).

ensureTalking(UUID1, UUID2, Timeout) ->
	call:execute(UUID1, "playback", "tone_stream://%(3000, 0, 2600)"),
	call:detect_tone(UUID2, "2600"),
	call:wait_event(UUID2, #{ "Event-Name" => "DETECTED_TONE" }, Timeout),
	call:stop_detect_tone(UUID2).

