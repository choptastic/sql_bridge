-module(sql_bridge_worker_db).

-behaviour(gen_server).
-compile({no_auto_import, [get/1]}).

-export([start_link/0]).
-export([
    set/2,
    get/1,
    clear/1
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, sql_bridge_worker_db_mapper).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(Worker) ->
    case ets:lookup(?TABLE, Worker) of
        [] -> undefined;
        [{Worker, DB}] -> DB
    end.

set(Worker, DB) ->
    case get(Worker) of
        DB ->
            ok;
        _ ->
            ets:insert(?TABLE, {Worker, DB}),
            updated
    end.

clear(Worker) ->
    ets:delete(?TABLE, Worker).



init([]) ->
    Config = [
        named_table,
        {read_concurrency, true},
        public,
        {write_concurrency, true}
    ],
    try ets:new(?TABLE, Config) of
        ?TABLE -> ok
    catch
        error:badarg ->
            case lists:member(?TABLE, ets:all()) of
                true ->
                    Owner = proplists:get_value(owner, ets:info(?TABLE)),
                    case Owner == self() of
                        true ->
                            error_logger:info_msg("sql_bridge: Trying to create ETS table (~p) that already exists, but we are already the owner, so that's okay.",[?TABLE]);
                        false ->
                            error_logger:error_msg("sql_bridge: Trying to create an ETS table (~p) that exists, and we aren't the owner. We are ~p and the owner is ~p. Time to crash.",[?TABLE, self(), Owner]),
                            erlang:exit({ets_table_exists, ?TABLE})
                    end;
                false ->
                    erlang:exit({failed_to_create_ets_table, {error, badarg}})
            end
    end,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.



