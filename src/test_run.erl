-module(test_run).
-behaviour(gen_server).

-export([start/1, start_link/1, stop/1, run/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {admin}).

start(Admin) -> gen_server:start(?MODULE, [Admin], []).
start_link(Admin) -> gen_server:start_link(?MODULE, [Admin], []).

run(Pid, Test) -> gen_server:call(Pid, {run, Test}, infinity).
stop(Pid) -> gen_server:stop(Pid).

init([Admin]) ->
	lager:info("start, admin:~p", [Admin]),
	{ok, Pid} = admin_user:start_link(Admin),
	erlang:link(Pid),
	{ok, #state{admin=Admin}}.

handle_cast(stop, S) -> {stop, normal, S};
handle_cast(_Msg, S=#state{}) -> lager:error("unhandled cast:~p", [_Msg]), {noreply, S}.

handle_info(_Info, S=#state{}) -> lager:error("unhandled info:~p", [_Info]), {noreply, S}.

handle_call({run, Test}, _From, S=#state{}) ->
	Test:main(),
	{reply, ok, S};

handle_call(_Request, _From, S=#state{}) -> lager:error("unhandled call:~p", [_Request]), {reply, ok, S}.
terminate(_Reason, _S) -> lager:info("terminate, reason:~p", [_Reason]), ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
