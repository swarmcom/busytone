-module(call_manager).
-behaviour(gen_server).

% accept and manage incoming agent callls

-export([start_link/0, originate/3, originate/2, originate/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

originate(URL, Exten, Opts) -> gen_server:call(?MODULE, {originate, URL, Exten, Opts}).
originate(URL, Opts) -> gen_server:call(?MODULE, {originate, URL, self_exten(), Opts}).
originate(URL) -> gen_server:call(?MODULE, {originate, URL, self_exten(), []}).

self_exten() -> io_lib:format("&erlang('~s:! ~s')", [?MODULE, node()]).

init([]) ->
	{ok, #state{}}.

handle_info({freeswitch_sendmsg, UUID}, #state{}=S) ->
	lager:notice("incoming message, uuid:~s", [UUID]),
	{ok, _Pid} = call:start_link(UUID),
	{noreply, S};

handle_info({get_pid, UUID, Ref, From}, #state{}=S) ->
	lager:notice("call control request, uuid:~s", [UUID]),
	CallPid =
		case call:pid(UUID) of
			undefined -> {ok, Pid} = call:start_link(UUID), Pid;
			Pid -> Pid
		end,
	From ! {Ref, CallPid},
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_call({originate, URL, Exten, Opts}, _From, S=#state{}) ->
	Re = 
		case fswitch:api("originate ~s~s ~s", [stringify_opts(Opts), URL, Exten]) of
			{ok, "+OK "++UUID} ->
				UUID1 = trim(UUID),
				{ok, UUID1};
			_Err -> _Err
		end,
	{reply, Re, S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

trim(Str) -> string:left(Str, erlang:length(Str)-1).

stringify_opts([]) -> "";
stringify_opts(Opts) when is_list(Opts) ->
	Str = string:join([ io_lib:format("~s=~s", [K,V]) || {K, V} <- Opts ], ","),
	io_lib:format("{~s}", [Str]).
