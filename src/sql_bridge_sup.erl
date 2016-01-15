-module(sql_bridge_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%% Starting simple, and we'll be dynamically adding pools
	{ok, {{one_for_one, 10, 10}, []}}.
