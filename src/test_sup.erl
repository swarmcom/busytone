-module(test_sup).
-behaviour(gen_server).

-export([start_link/0, run/0, run/1, info/0, debug/0, notice/0, set_loglevel/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

info() -> set_loglevel(info).
debug() -> set_loglevel(debug).
notice() -> set_loglevel(notice).

set_loglevel(Level) ->
	lager:set_loglevel(lager_console_backend, Level).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run(Test) -> gen_server:call(?MODULE, {run, Test}, infinity).
run() -> gen_server:call(?MODULE, {run}, infinity).

init([]) ->
	lager:notice("start", []),
	process_flag(trap_exit, true),
	{ok, #state{}}.
handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'EXIT', _PidOfTest, normal}, S=#state{}) ->
	{noreply, S};

handle_info({'EXIT', _PidOfTest, Error}, S=#state{}) ->
	lager:error("test failed:~p", [Error]),
	{noreply, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({run, Test}, _From, S=#state{}) ->
	run_test(Test),
	{reply, ok, S};

handle_call({run}, _From, S=#state{}) ->
	AllMods = [ erlang:atom_to_list(Module) || {Module, _} <- code:all_loaded() ],
	TestMods = [ erlang:list_to_atom(Module) || Module <- AllMods, is_test(Module) ],
	[ run_test(Test) || Test <- TestMods ],
	{reply, ok, S};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.
terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

run_test(Test) ->
	{ok, Pid} = test_run:start_link(),
	Re = test_run:run(Pid, Test),
	lager:notice("test:~p result:~p", [Test, Re]),
	test_run:stop(Pid).

is_test("t_"++_) -> true;
is_test(_) -> false.
