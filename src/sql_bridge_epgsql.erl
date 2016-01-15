-module(sql_bridge_epgsql).
-behaviour(sql_bridge_adapter).

-export([start/0,
		 connect/5,
		 query/3,
		 query/4,
		 encode/1]).

start() ->
	application:start(poolboy),
	%application:start(epgsql),
	ok.

connect(DB, User, Pass, Host, Port) when is_atom(DB) ->
	WorkerArgs = [
		{hostname, Host},
		{username, User},
		{password, Pass},
		{port, Port}
	],
	sql_bridge_utils:start_poolboy_pool(DB, WorkerArgs, sql_bridge_epgsql_worker).

query(Type, DB, Q) ->
	query(Type, DB, Q, []).

query(Type, DB, Q, ParamList) ->
	try query_catched(Type, DB, Q, ParamList)
	catch
		exit:{noproc, _} ->
			{error, no_pool}
	end.

query_catched(Type, DB, Q, ParamList) ->
	ToRun = fun(Worker) ->
		%% calls sql_bridge_epgsql_worker:handle_call()
		gen_server:call(Worker, {equery, Q, ParamList})
	end,
	Res = sql_bridge_utils:with_poolboy_pool(DB, ToRun),
	{ok, format_result(Type, Res)}.

format_result(UID, {ok, Count}) when UID=:=update;
									 UID=:=insert;
									 UID=:=delete ->
	Count;
format_result(select, {ok, Columns, Rows}) ->
	ColNames = extract_colnames(Columns),
	[make_proplist(ColNames, Row) || Row <- Rows].
	
extract_colnames(Columns) ->
	[list_to_atom(binary_to_list(CN)) || {column, CN, _, _, _, _} <- Columns].


make_proplist(Columns, Row) when is_tuple(Row) ->
	make_proplist(Columns, tuple_to_list(Row));
make_proplist([Col|Cols], [Val|Vals]) ->
	[{Col, Val} | make_proplist(Cols, Vals)];
make_proplist([], []) ->
	[].

encode(Val) ->
	Val.
