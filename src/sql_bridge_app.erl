-module(sql_bridge_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	sql_bridge_sup:start_link().

stop(_State) ->
        ok.

