-module(admin).
-behaviour(gen_server).

-export([start_link/1,
	new_agent/0, new_agent/1, get_agent/1, update_agent/2,
	get_profile/1, new_profile/0, new_profile/1, update_profile/2,
	new_queue/0, new_queue/1, get_queue/1, update_queue/2,
	new_group/0, new_group/1, get_group/1, update_group/2,
	rpc_call/2, call/2, wait_ws/1,
	stop/0, reset/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user, agent=1, profile=1, queue=1, group=1, watch=#{}}).

start_link(Admin) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Admin], []).

new_agent() -> new_agent(#{ skills => #{ english => true } }).
new_agent(M) -> gen_server:call(?MODULE, {new_agent, M}).
new_profile() -> new_profile(#{}).
new_profile(M) -> gen_server:call(?MODULE, {new_profile, M}).
new_queue() -> new_queue(#{}).
new_queue(M) -> gen_server:call(?MODULE, {new_queue, M}).
new_group() -> new_group(#{}).
new_group(M) -> gen_server:call(?MODULE, {new_group, M}).
get_agent(Login) -> gen_server:call(?MODULE, {get_agent, Login}).
get_profile(Name) -> gen_server:call(?MODULE, {get_profile, Name}).
get_queue(Name) -> gen_server:call(?MODULE, {get_queue, Name}).
get_group(Name) -> gen_server:call(?MODULE, {get_group, Name}).
update_agent(Login, Props) -> gen_server:call(?MODULE, {update_agent, Login, Props}).
update_profile(Name, Props) -> gen_server:call(?MODULE, {update_profile, Name, Props}).
update_queue(Name, Props) -> gen_server:call(?MODULE, {update_queue, Name, Props}).
update_group(Name, Props) -> gen_server:call(?MODULE, {update_group, Name, Props}).

rpc_call(Cmd, Args) -> gen_server:call(?MODULE, {rpc_call, Cmd, Args}).
call(Cmd, Args) -> gen_server:call(?MODULE, {rpc_call, <<"ouc_rpc_adm.",(a2b(Cmd))/binary>>, Args}).
wait_ws(Mask) -> gen_server:call(?MODULE, {wait_ws, Mask}).

a2b(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
a2b(B) when is_binary(B) -> B.

stop() -> gen_server:cast(?MODULE, {stop}).
reset() -> gen_server:call(?MODULE, {reset}).

init([{Login, Pass}=_A]) ->
	lager:info("start, admin:~p", [_A]),
	Admin = test_lib:login(Login, Pass, Login),
	agent:rpc_call(Admin, <<"ouc_rpc_adm.reset">>, []),
	{ok, #state{user=Admin, watch=#{ erlang:monitor(process, agent:pid(Admin)) => {admin, Admin} }}}.

handle_cast({stop}, S=#state{}) ->
	{stop, normal, S};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{user=Admin, watch=W}) ->
	delete(Admin, maps:get(Ref, W, undefined)),
	{noreply, S#state{watch=maps:remove(Ref, W)}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({new_agent, Map}, {Pid, _Ref}, S=#state{agent=Id, user=Admin, watch=W}) ->
	AgentId = <<"test_agent_", (erlang:integer_to_binary(Id))/binary>>,
	[Login, Password, Number] = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_agent">>, [Map#{ id => AgentId }]),
	Agent = agent_sup:agent(Pid, Login, Password, Number),
	agent:wait_ws(Agent, #{ <<"username">> => Agent }),
	{reply, Agent, S#state{agent=Id+1, watch=W#{ erlang:monitor(process, Pid) => {agent, Agent} }}};

handle_call({new_profile, M}, {Pid, _Ref}, S=#state{profile=Id, user=Admin, watch=W}) ->
	ProfileId = <<"test_profile_", (erlang:integer_to_binary(Id))/binary>>,
	Name = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_profile">>, [M#{ id => ProfileId }]),
	{reply, Name, S#state{profile=Id+1, watch=W#{ erlang:monitor(process, Pid) => {profile, Name} }}};

handle_call({new_queue, M}, {Pid, _Ref}, S=#state{queue=Id, user=Admin, watch=W}) ->
	QueueId = <<"test_queue_", (erlang:integer_to_binary(Id))/binary>>,
	Name = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_queue">>, [M#{ name => QueueId }]),
	{reply, Name, S#state{queue=Id+1, watch=W#{ erlang:monitor(process, Pid) => {queue, Name} }}};

handle_call({new_group, M}, {Pid, _Ref}, S=#state{group=Id, user=Admin, watch=W}) ->
	GroupId = <<"test_queue_group_", (erlang:integer_to_binary(Id))/binary>>,
	Name = agent:rpc_call(Admin, <<"ouc_rpc_adm.create_group">>, [M#{ name => GroupId }]),
	{reply, Name, S#state{group=Id+1, watch=W#{ erlang:monitor(process, Pid) => {group, Name} }}};

handle_call({get_agent, Login}, _, S=#state{user=Admin}) ->
	Agent = agent:rpc_call(Admin, <<"ouc_rpc_adm.get_agent">>, [Login]),
	{reply, Agent, S};

handle_call({get_profile, Name}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.get_profile">>, [Name]),
	{reply, Re, S};

handle_call({get_queue, Name}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.get_queue">>, [Name]),
	{reply, Re, S};

handle_call({get_group, Name}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.get_group">>, [Name]),
	{reply, Re, S};

handle_call({update_agent, Login, Diff}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.update_agent">>, [Login, Diff]),
	{reply, Re, S};

handle_call({update_profile, Name, Diff}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.update_profile">>, [Name, Diff]),
	{reply, Re, S};

handle_call({update_queue, Name, Diff}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.update_queue">>, [Name, Diff]),
	{reply, Re, S};

handle_call({update_group, Name, Diff}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, <<"ouc_rpc_adm.update_group">>, [Name, Diff]),
	{reply, Re, S};

handle_call({rpc_call, Cmd, Args}, _, S=#state{user=Admin}) ->
	Re = agent:rpc_call(Admin, Cmd, Args),
	{reply, Re, S};

handle_call({wait_ws, Mask}, _, S=#state{user=Admin}) ->
	Re = agent:wait_ws(Admin, Mask),
	{reply, Re, S};

handle_call({reset}, _, #state{user=Admin, watch=W}) ->
	call:hupall(),
	ts_core:wait(fun() -> [] = call:active() end),
	[ delete(Admin, Value) || Value <- maps:values(W) ],
	{reply, ok, #state{user=Admin}};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{user=Admin, watch=W}) ->
	lager:info("terminate, reason:~p", [_Reason]),
	call:hupall(),
	[ delete(Admin, Value) || Value <- maps:values(W) ],
	ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.


delete(Admin, {group, Group}) -> <<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_group">>, [Group]);
delete(Admin, {queue, Queue}) -> <<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_queue">>, [Queue]);
delete(Admin, {agent, Agent}) -> <<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_agent">>, [Agent]);
delete(Admin, {profile, Profile}) -> <<"ok">> = agent:rpc_call(Admin, <<"ouc_rpc_adm.delete_profile">>, [Profile]);
delete(_, {admin, _}) -> admin:stop();
delete(_, undefined) -> skip.
