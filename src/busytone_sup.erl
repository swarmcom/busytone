-module(busytone_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

-define(CHILD(M, A), #{ id => M, start => {M, start_link, A}, restart => permanent, shutdown => 2000, type => worker, modules => [M]}).

start_link() ->
	supervisor:start_link(?MODULE, []).

init(_Args) ->
	lager:notice("start"),
	SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
	ChildSpecs = [
	],
	{ok, {SupFlags, ChildSpecs}}.