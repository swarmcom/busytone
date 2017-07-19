-module(watcher).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mfa, timeout, pid}).

start_link(MFA) -> start_link(MFA, 2000).
start_link(MFA, Timeout) -> gen_server:start_link(?MODULE, {MFA, Timeout}, []).

start_after(T) -> erlang:send_after(T, self(), start).

init({MFA, Timeout}) ->
	lager:notice("start, timeout:~p mfa:~p", [Timeout, MFA]),
	process_flag(trap_exit, true),
	start_after(0),
	{ok, #state{mfa=MFA, timeout=Timeout}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'EXIT', _FromPid, Reason}, S=#state{timeout=T}) ->
	lager:info("child is dead, reason:~p:", [Reason]),
	start_after(T),
	{noreply, S};

handle_info(start, S=#state{mfa={M, F, A}}) ->
	{ok, Pid} = erlang:apply(M, F, A),
	{noreply, S#state{pid=Pid}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(_Request, _From, S=#state{}) ->
	{reply, ok, S}.
terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
