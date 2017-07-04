-module(fswitch).
-behaviour(gen_server).

-export([start_link/1, api/2, api/1, bgapi/2, bgapi/1, command/3, parse_uuid_dump/1, parse_uuid_dump_string/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	drone
}).

start_link(Drone) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Drone], []).

api(Cmd, Args) -> gen_server:call(?MODULE, {api, Cmd, Args}).
api(Cmd) -> gen_server:call(?MODULE, {api, Cmd, []}).
bgapi(Cmd, Args) -> gen_server:call(?MODULE, {bgapi, Cmd, Args}).
bgapi(Cmd) -> gen_server:call(?MODULE, {bgapi, Cmd, []}).
command(UUID, Command, Args) -> gen_server:call(?MODULE, {command, UUID, Command, Args}).

init([FsDrone]) ->
	Drone = erlang:list_to_atom(FsDrone),
	lager:notice("start, drone:~p", [Drone]),
	net_adm:ping(Drone),
	erlang:monitor_node(Drone, true),
	{ok, #state{drone=Drone}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({nodedown, Node}, #state{}=S) ->
	lager:error("freeswitch node down:~p", [Node]),
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({api, Cmd, Args}, _From, S=#state{drone=Fs}) ->
	{reply, freeswitch:fapi(Fs, Cmd, Args), S};

handle_call({bgapi, Cmd, Args}, _From, S=#state{drone=Fs}) ->
	{reply, freeswitch:fbgapi(Fs, Cmd, Args), S};

handle_call({command, UUID, Cmd, Args}, _From, S=#state{drone=Fs}) ->
	{reply, freeswitch:sendmsg(Fs, UUID, [
		{"call-command", "execute"},
		{"execute-app-name", Cmd},
		{"execute-app-arg", Args}
	]), S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) ->
	{ok, S}.

is_variable({K, _}) -> is_variable(K);
is_variable("variable_"++_Var) -> true;
is_variable(_X) -> false.

variable("variable_"++Var) -> Var.

parse_uuid_dump_string(Str) ->
	Tokens = string:tokens(Str, "\n"),
	[ erlang:list_to_tuple(string:tokens(Token, ": ")) || Token <- Tokens ].

parse_uuid_dump(Pairs) ->
	{Variables, Vars} = lists:partition(fun is_variable/1, Pairs),
	VarMap = lists:foldl(fun({K,V}, M) -> M#{ K => V} end, #{}, Vars),
	VariableMap = lists:foldl(fun({K,V}, M) -> M#{ variable(K) => V} end, #{}, Variables),
	{VarMap, VariableMap}.