-module(busytone_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-include_lib("busytone/include/busytone.hrl").

-define(CHILD(I, M, A), #{ id => I, start => {M, start_link, A}, restart => permanent, shutdown => 2000, type => worker, modules => []}).
-define(CHILD(M, A), #{ id => M, start => {M, start_link, A}, restart => permanent, shutdown => 2000, type => worker, modules => []}).

cfg(K) -> {ok, V} = application:get_env(busytone, K), V.

start_link() ->
	supervisor:start_link(?MODULE, []).

init(_Args) ->
	lager:notice("start"),
	SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
	Host = cfg(reach_host),
	Port = cfg(reach_port),
	Agents = cfg(agents),
	Fs = cfg(freeswitch_drone),
	ChildSpecs =
		[ ?CHILD(call_manager, []), ?CHILD(fswitch, [Fs]) ]
		++ [ ?CHILD(Login, agent, [Host, Port, A]) || A=#agent{login=Login} <- Agents ],
	{ok, {SupFlags, ChildSpecs}}.