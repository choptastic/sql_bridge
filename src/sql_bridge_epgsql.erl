-module(sql_bridge_epgsql).
-behaviour(sql_bridge_adapter).
-include("compat.hrl").

-export([start/0,
		 connect/5,
		 query/4,
 		 schema_db_column/0,
		 encode/1,
		 start_transaction/1,
		 commit_transaction/1,
		 rollback_transaction/1,
		 with_transaction/2]).

start() ->
	application:start(poolboy),
	ok.

connect(DB, User, Pass, Host, Port) when is_atom(DB) ->
	WorkerArgs = [
		{database, atom_to_list(DB)},
		{hostname, Host},
		{username, User},
		{password, Pass},
		{port, Port}
	],
	sql_bridge_utils:start_poolboy_pool(DB, WorkerArgs, sql_bridge_epgsql_worker),
	ok.

start_transaction(DB) ->
	sql_bridge_utils:checkout_pool(DB),
	query(none, DB, "BEGIN", []),
	ok.

rollback_transaction(DB) ->
	query(none, DB, "ROLLBACK", []),
	sql_bridge_utils:checkin_pool(DB),
	ok.

commit_transaction(DB) ->
	query(none, DB, "COMMIT", []),
	sql_bridge_utils:checkin_pool(DB),
	ok.

with_transaction(DB, Fun) ->
	start_transaction(DB),
	try Fun() of
		Res -> 
			commit_transaction(DB),
			Res
	catch
		Error:Class ->
			rollback_transaction(DB),
			{error, [{Error, Class}, erlang:get_stacktrace()]}
	end.

query(Type, DB, Q, ParamList) ->
	try query_catched(Type, DB, Q, ParamList)
	catch
		exit:{noproc, _} ->
			{error, no_pool}
	end.
	
query_catched(Type, DB, Q, ParamList) ->
	{Q2, ParamList2} = maybe_replace_tokens(Q, ParamList),
	ToRun = fun(Worker) ->
		%% calls sql_bridge_epgsql_worker:handle_call()
		InnerRes = gen_server:call(Worker, {equery, Q2, ParamList2}),
		case {Type, InnerRes} of
			{insert, {ok, _Count}} ->
				InsertRes = gen_server:call(Worker, {equery, <<"select lastval();">>, []}),
				{ok, format_insert_id(InsertRes)};
			_ ->
				InnerRes
		end
	end,

	Res = sql_bridge_utils:with_poolboy_pool(DB, ToRun),
	{ok, format_result(Type, Res)}.
	
format_insert_id({ok, _Columns, Rows}) ->
	case format_lists(Rows) of
		[[Insertid]] -> Insertid;
		_ -> undefined
	end.

maybe_replace_tokens(Q, ParamList) ->
	case sql_bridge_utils:replacement_token() of
		postgres -> {Q, ParamList};
		mysql -> sql_bridge_utils:token_mysql_to_postgres(Q, ParamList)
	end.

format_result(none, _) ->
	ok;
format_result(_Type, {ok, Count}) ->
	Count;
format_result(insert, {ok, _Count, _Columns, Rows}) ->
	element(1,hd(Rows));
format_result(Type, {ok, _Count, Columns, Rows}) ->
	format_result(Type, {ok, Columns, Rows});
format_result(tuple, {ok, _Columns, Rows}) ->
	format_tuples(Rows);
format_result(list, {ok, _Columns, Rows}) ->
	format_lists(Rows);
format_result(proplist, {ok, Columns, Rows}) ->
	format_proplists(Columns, Rows);
format_result(dict, {ok, Columns, Rows}) ->
	format_dicts(Columns, Rows);
format_result(map, {ok, Columns, Rows}) ->
	format_maps(Columns, Rows).

format_tuples(Rows) ->
	case sql_bridge_utils:stringify_binaries() of
		true ->
			[list_to_tuple(format_list(Row)) || Row <- Rows];
		false ->
			Rows
	end.

format_lists(Rows) ->
	case sql_bridge_utils:stringify_binaries() of
		true ->
			[format_list(Row) || Row <- Rows];
		false ->
			[tuple_to_list(Row) || Row <- Rows]
	end.

format_list(Row) when is_tuple(Row) ->
	Row2 = tuple_to_list(Row),
	[sql_bridge_stringify:maybe_string(V) || V <- Row2].

format_proplists(Columns, Rows) ->
	ColNames = extract_colnames(Columns),
	[make_proplist(ColNames, Row) || Row <- Rows].

format_dicts(Columns, Rows) ->
	ColNames = extract_colnames(Columns),
	[make_dict(ColNames, Row) || Row <- Rows].

make_dict(Cols, Row) when is_tuple(Row) ->
	make_dict(Cols, tuple_to_list(Row), dict:new()).

make_dict([], [], Dict) ->
	Dict;
make_dict([Col|Cols], [Val|Vals], Dict) ->
	Val2 = sql_bridge_stringify:maybe_string(Val),
	NewDict = dict:store(Col, Val2, Dict),
	make_dict(Cols, Vals, NewDict).

	
extract_colnames(Columns) ->
	[list_to_atom(binary_to_list(CN)) || {column, CN, _, _, _, _} <- Columns].


make_proplist(Columns, Row) when is_tuple(Row) ->
	make_proplist(Columns, tuple_to_list(Row));
make_proplist([Col|Cols], [Val|Vals]) ->
	Val2 = sql_bridge_stringify:maybe_string(Val),
	[{Col, Val2} | make_proplist(Cols, Vals)];
make_proplist([], []) ->
	[].

-ifdef(has_maps).
format_maps(Columns, Rows) ->
	ColNames = extract_colnames(Columns),
	[make_map(ColNames, Row) || Row <- Rows].

make_map(Cols, Row) ->
	make_map(Cols, tuple_to_list(Row), maps:new()).

make_map([], [], Map) ->
	Map;
make_map([Col|Cols],[Val|Vals], Map) ->
	Val2 = sql_bridge_stringify:maybe_string(Val),
	NewMap = maps:put(Col, Val2, Map),
	make_map(Cols, Vals, NewMap).

-else.
format_maps(_,_) ->
	throw(maps_not_supported).
-endif.

schema_db_column() ->
	"table_catalog".

encode(A) when is_atom(A) ->
	encode(A);
encode(I) when is_integer(I) ->
	list_to_binary(integer_to_list(I));
encode(F) when is_float(F) ->
	list_to_binary(float_to_list(F));
encode(B) when is_binary(B) ->	
	<<"'",(escape_binary(B, <<>>))/binary,"'">>;
encode(L) when is_list(L) ->
	encode(list_to_binary(L)).

escape_binary(<<X/utf8, Rest/binary>>, Acc) when X==$' ->
	escape_binary(Rest, <<Acc/binary,"''">>);
escape_binary(<<X/utf8, Rest/binary>>, Acc) ->
	escape_binary(Rest, <<Acc/binary,X/utf8>>);
escape_binary(<<>>, Acc) ->
	Acc.

