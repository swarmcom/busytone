-module(busytone_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_Type, _Args) ->
	busytone_sup:start_link().

start() ->
	ok.

stop(_State) ->
	ok.
