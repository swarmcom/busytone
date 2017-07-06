-module(event_log).
-behaviour(gen_server).

-export([start_link/0, add/2, wait/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	wait,
	log = []
}).

start_link() -> gen_server:start_link(?MODULE, [], []).
add(Id, Msg) -> gen_server:call(Id, {add, Msg}).
wait(Id, Match, Caller) -> gen_server:call(Id, {wait, Match, Caller}).

init([]) -> {ok, #state{
	log = []
}}.

handle_call({add, Msg}, _, S=#state{ wait = undefined }) ->
	{reply, ok, add_msg(Msg, S)};

handle_call({add, Msg}, _, S=#state{ wait = {Match, Caller} }) ->
	case util:match_maps(Match, Msg) of
		true -> {reply, {match, Caller, {erlang:timestamp(), Msg}}, (add_msg(Msg, S))#state{ wait = undefined }};
		false -> {reply, no_match, add_msg(Msg, S)}
	end;

handle_call({wait, Match, Caller}, _, S=#state{ log = Log }) ->
	case lookback(Match, Log) of
		{found, _, _}=Msg -> {reply, Msg, S};
		_ -> {reply, no_match, S#state{ wait = {Match,Caller} } }
	end;

handle_call(_Request, _From, S=#state{}) -> {reply, {error, not_handled}, S}.

handle_cast(_Msg, S=#state{}) -> {noreply, S}.
handle_info(_Info, S=#state{}) -> {noreply, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

lookback(Match, Log) -> lookback(5, Match, Log).
lookback(0, _, _) -> not_found;
lookback(_, _, []) -> not_found;
lookback(Depth, Match, [{Ts,Msg} | Log]) ->
	case util:match_maps(Match, Msg) of
		true -> {match, Ts, Msg};
		false -> lookback(Depth-1, Match, Log)
	end.

add_msg(Msg, S=#state{log=Log}) -> S#state{ log = [ {erlang:timestamp(), Msg} | Log ] }.
