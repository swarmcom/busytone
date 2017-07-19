-module(admin_user).
-behaviour(gen_server).

-export([start_link/1, new_agent/0, new_agent/1, new_profile/0, new_profile/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user, parent, profiles=[]}).

start_link(Admin) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [self(), Admin], []).

new_agent() -> new_agent(#{ skills => #{ english => true } }).
new_agent(M) -> gen_server:call(?MODULE, {new_agent, M}).
new_profile() -> new_profile(#{}).
new_profile(M) -> gen_server:call(?MODULE, {new_profile, M}).

init([Parent, {Login, Pass}=_A]) ->
	lager:notice("start, parent pid:~p admin:~p", [Parent, _A]),
	Admin = test_lib:login(Login, Pass, Login),
	erlang:monitor(process, Parent),
	{ok, #state{user=Admin, parent=Parent}}.
handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'DOWN', _Ref, process, Pid, normal}, S=#state{parent=Pid}) ->
	{stop, normal, S};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({new_agent, Map}, _, S=#state{user=Admin}) ->
	[Login, Password, Number] = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_test_agent">>, [Map]),
	Agent = test_lib:login(Login, Password, Number),
	agent:auto_delete(Agent, true),
	{reply, Agent, S};

handle_call({new_profile, M}, _, S=#state{user=Admin, profiles=Profiles}) ->
	Name = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_test_profile">>, [M]),
	{reply, Name, S#state{profiles=[Name|Profiles]}};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.
terminate(_Reason, _S=#state{profiles=Profiles, user=Admin}) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	[ delete_profile(Admin, Profile) || Profile <- Profiles ],
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

delete_profile(Admin, Profile) ->
	<<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_profile">>, [Profile]).
