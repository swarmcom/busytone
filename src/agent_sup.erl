-module(agent_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, agent/1, agent/3]).
-include_lib("busytone/include/busytone.hrl").

-define(CHILD(I, M, A), #{ id => I, start => {M, start_link, A}, restart => permanent, shutdown => 2000, type => worker, modules => []}).

cfg(K) -> {ok, V} = application:get_env(busytone, K), V.

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

agent(#agent{login=Login}=A) ->
	supervisor:start_child(?MODULE, ?CHILD(Login, agent, [cfg(reach_host), cfg(reach_port), A])).

agent(Login, Password, Number) -> agent(#agent{login=Login, password=Password, number=Number}).

init(_Args) ->
	lager:notice("start"),
	SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
	Agents = cfg(agents),
	Host = cfg(reach_host),
	Port = cfg(reach_port),
	ChildSpecs = [ ?CHILD(Login, agent, [Host, Port, A]) || A=#agent{login=Login} <- Agents ],
	{ok, {SupFlags, ChildSpecs}}.