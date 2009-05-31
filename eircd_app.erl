-module(eircd_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	eircd:init(),
	ok.

stop(_State) ->
	ok.

