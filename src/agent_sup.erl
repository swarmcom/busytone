-module(agent_sup).
-behaviour(gen_server).
-include_lib("busytone/include/busytone.hrl").

-export([start_link/2, agent/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	host,
	port
}).

start_link(Host, Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).
agent(Login, Password, Number) -> gen_server:call(?MODULE, #agent{login=Login, password=Password, number=Number}).

init([Host, Port]) ->
	lager:notice("start, host:~p port:~p", [Host, Port]),
	{ok, #state{host=Host, port=Port}}.
handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.
handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(A=#agent{login=Login}, {Pid, _Ref}, S=#state{host=Host, port=Port}) ->
	agent:start(Pid, Host, Port, A),
	{reply, Login, S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.
terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
