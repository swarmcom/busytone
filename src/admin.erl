-module(admin).
-behaviour(gen_server).

-export([start_link/1,
	create/1, create/2, get/1, get/2, update/1, update/2, delete/1, delete/2,
	call/2, call/3, wait_ws/1,
	stop/0, reset/0,
	agents_queue/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user, ids=#{}, watch=#{}}).

start_link(Admin) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Admin], []).

create(Entity) -> create(Entity, #{}).
create(Entity, M) -> gen_server:call(?MODULE, {create, Entity, M}).

get(Entity) -> get(Entity, #{}).
get(Entity, Id) -> gen_server:call(?MODULE, {get, Entity, Id}).

update(Entity) -> update(Entity, #{}).
update(Entity, Id) -> gen_server:call(?MODULE, {update, Entity, Id}).

delete(Entity) -> delete(Entity, #{}).
delete(Entity, Id) -> gen_server:call(?MODULE, {delete, Entity, Id}).

call(F, A) -> gen_server:call(?MODULE, {call, ws_admin, F, A}).
call(M, F, A) -> gen_server:call(?MODULE, {call, M, F, A}).

wait_ws(Mask) -> gen_server:call(?MODULE, {wait_ws, Mask}).

stop() -> gen_server:cast(?MODULE, {stop}).
reset() -> gen_server:call(?MODULE, {reset}).

agents_queue() -> call(agents, []).

init([{Login, Pass}=_A]) ->
	lager:info("start, admin:~p", [_A]),
	Admin = agent_sup:agent(Login, Pass),
	{ok, #state{user=Admin, watch=#{ erlang:monitor(process, agent:pid(Admin)) => {admin, Admin} }}}.

handle_cast({stop}, S=#state{}) ->
	{stop, normal, S};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, S=#state{user=Admin, watch=W}) ->
	cleanup_waiter(Admin, maps:get(Ref, W, undefined)),
	{noreply, S#state{watch=maps:remove(Ref, W)}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

fmt(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).

entity_name(Entity, Id) -> erlang:list_to_binary(fmt("test_~s_~p", [Entity, Id])).
entity_module(Entity) -> erlang:list_to_binary(fmt("ws_db_~s", [Entity])).

handle_call({create, Entity, Map}, {Pid, _Ref}, S=#state{user=Admin, ids=Ids, watch=W}) ->
	EntityId = maps:get(Entity, Ids, 1),
	Name = entity_name(Entity, EntityId),
	Re = agent:call(Admin, entity_module(Entity), create, [Map#{ name => Name }]),
	Id = make_id(Re),
	handle_created(Entity, Id, S),
	{reply, Id, S#state{ids=Ids#{ Entity => EntityId + 1 }, watch=W#{ erlang:monitor(process, Pid) => {Entity, Id} }}};

handle_call({get, Entity, Id}, _From, S=#state{user=Admin}) ->
	Re = agent:call(Admin, entity_module(Entity), get, [Id]),
	{reply, Re, S};

handle_call({get, Entity}, _From, S=#state{user=Admin}) ->
	Re = agent:call(Admin, entity_module(Entity), get, []),
	{reply, Re, S};

handle_call({update, Entity, M}, _From, S=#state{user=Admin}) ->
	Re = agent:call(Admin, entity_module(Entity), update, [M]),
	{reply, Re, S};

handle_call({call, Module, Cmd, Args}, _, S=#state{user=Admin}) ->
	Re = agent:call(Admin, Module, Cmd, Args),
	{reply, Re, S};

handle_call({wait_ws, Mask}, _, S=#state{user=Admin}) ->
	Re = agent:wait_ws(Admin, Mask),
	{reply, Re, S};

handle_call({reset}, _, #state{user=Admin, watch=W}) ->
	call:hupall(),
	ts_core:wait(fun() -> [] = call:active() end),
	{reply, ok, #state{user=Admin, watch=W}};

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S=#state{user=Admin, watch=W}) ->
	lager:info("terminate, reason:~p", [_Reason]),
	call:stop(),
	cleanup_waiters(Admin, W),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

cleanup_waiters(Admin, W) ->
	[ cleanup_waiter(Admin, Value) || Value = {Type, _} <- maps:values(W), Type =/= admin ].

cleanup_waiter(Admin, {agent=Entity, Id}) ->
	agent:call(Id, ws_agent, stop, []),
	<<"ok">> = agent:call(Admin, entity_module(Entity), delete, [Id]);
cleanup_waiter(Admin, {Entity, Id}) ->
	<<"ok">> = agent:call(Admin, entity_module(Entity), delete, [Id]);
cleanup_waiter(_, undefined) -> skip.

handle_created(agent=Entity, Id, #state{user=Admin}) ->
	#{ <<"login">> := Login, <<"password">> := Password } = agent:call(Admin, entity_module(Entity), get, [Id]),
	agent_sup:agent(Login, Password);
handle_created(_, _, _) ->
	ok.

make_id(#{ <<"id">> := Id }) -> Id;
make_id(Id) -> Id.
