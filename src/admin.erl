-module(admin).
-behaviour(gen_server).

-export([start_link/1, new_agent/0, new_agent/1, new_profile/0, new_profile/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user, agent=1, profile=1, watch=#{}}).

start_link(Admin) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Admin], []).

new_agent() -> new_agent(#{ skills => #{ english => true } }).
new_agent(M) -> gen_server:call(?MODULE, {new_agent, M}).
new_profile() -> new_profile(#{}).
new_profile(M) -> gen_server:call(?MODULE, {new_profile, M}).

stop() -> gen_server:cast(?MODULE, {stop}).

init([{Login, Pass}=_A]) ->
	lager:info("start, admin:~p", [_A]),
	Admin = test_lib:login(Login, Pass, Login),
	{ok, #state{user=Admin}}.

handle_cast({stop}, S=#state{}) ->
	{stop, normal, S};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{user=Admin, watch=W}) ->
	delete(Admin, maps:get(Ref, W)),
	{noreply, S#state{watch=maps:remove(Ref, W)}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({new_agent, Map}, {Pid, _Ref}, S=#state{agent=Id, user=Admin, watch=W}) ->
	AgentId = <<"test_agent_", (erlang:integer_to_binary(Id))/binary>>,
	[Login, Password, Number] = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_test_agent">>, [Map#{ id => AgentId }]),
	Agent = agent_sup:agent(Pid, Login, Password, Number),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	{reply, Agent, S#state{agent=Id+1, watch=W#{ erlang:monitor(process, Pid) => {agent, Agent} }}};

handle_call({new_profile, M}, {Pid, _Ref}, S=#state{profile=Id, user=Admin, watch=W}) ->
	ProfileId = <<"test_profile_", (erlang:integer_to_binary(Id))/binary>>,
	Name = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_test_profile">>, [M#{ id => ProfileId }]),
	{reply, Name, S#state{profile=Id+1, watch=W#{ erlang:monitor(process, Pid) => {profile, Name} }}};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{user=Admin, watch=W}) ->
	lager:info("terminate, reason:~p", [_Reason]),
	[ delete(Admin, Value) || Value <- maps:values(W) ],
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

delete(Admin, {agent, Agent}) -> <<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_agent">>, [Agent]);
delete(Admin, {profile, Profile}) -> <<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_profile">>, [Profile]).
