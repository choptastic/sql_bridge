-module(sql_bridge_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	%% Starting simple, and we'll be dynamically adding pools
    SupFlags = #{
        strategy=>one_for_one,
        intensity=>2000,
        period=>1
    },
    Children = [
        #{
            id=>sql_bridge_worker_db,
            start=>{sql_bridge_worker_db, start_link, []}
        }
    ],
    {ok, {SupFlags, Children}}.
