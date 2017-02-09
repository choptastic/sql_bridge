-module(sql_bridge_mysql_otp).
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
		 with_transaction/2
		]).

%% 
-export([maybe_replace_tokens/2]).


start() ->
	application:start(poolboy),
	ok.

connect(DB, User, Pass, Host, Port) when is_atom(DB) ->
	WorkerArgs = [
		{database, atom_to_list(DB)},
		{host, Host},
		{user, User},
		{password, Pass},
		{port, Port}
	],
	sql_bridge_utils:start_poolboy_pool(DB, WorkerArgs, mysql),
	ok.

start_transaction(DB) ->
    case sql_bridge_utils:trans_depth(DB) of
        0 ->
            query(none, DB, "BEGIN", []);
        _ ->
            ok
    end,
    sql_bridge_utils:trans_deeper(DB),
    sql_bridge_utils:checkout_pool(DB),
    ok.

rollback_transaction(DB) ->
    sql_bridge_utils:trans_shallower(DB),
	query(none, DB, "ROLLBACK", []),
	sql_bridge_utils:checkin_pool(DB),
	ok.

commit_transaction(DB) ->
    case sql_bridge_utils:trans_depth(DB) of
        1 ->
            Res = query(none, DB, "COMMIT", []),
            sql_bridge_utils:clear_trans_depth(DB),
            Res;
        X when X > 1 ->
            sql_bridge_utils:trans_shallower(DB),
            ok %% mysql does not supported nested transactions, so we'll only commit on the last depth
    end,
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
            ErrMsg = [{Error, Class}, erlang:get_stacktrace()],
            wf:warning("Errored Query: ~p",[ErrMsg]),
			rollback_transaction(DB),
			{error, ErrMsg}
	end.

query(Type, DB, Q, ParamList) ->
	try query_catched(Type, DB, Q, ParamList)
	catch
		exit:{noproc, _} ->
            {error, no_pool};
        exit:{{{badmatch,{error,closed}}, _}, _} ->
            {error, disconnected};
        exit:{{{case_clause,{error,closed}}, _}, _} ->
            {error, disconnected};
        E:T ->
            error_logger:error_msg("Unhandled Error: ~p:~p", [E,T]),
            throw(unhandled_error)
	end.

query_catched(Type, DB, Q, ParamList) ->
	{Q2, ParamList2} = maybe_replace_tokens(Q, ParamList),
	ToRun = fun(Worker) ->
		Res = mysql_query(Worker, Q2, ParamList2),
        case Res of
            {error, Reason} ->
                error_logger:warning_msg("Error in Query.~nError: ~p~nQuery: ~p",[Reason, Q]);
            _ ->
                ok
        end,
		case Type of
			insert -> mysql:insert_id(Worker);
			update -> mysql:affected_rows(Worker);
			_ -> Res
		end
	end,
	case sql_bridge_utils:with_poolboy_pool(DB, ToRun) of
		{error, Reason} -> {error, Reason};
		Result ->
            {ok, format_result(Type, Result)}
	end.
	
mysql_query(Worker, Q, []) ->
	mysql:query(Worker, Q);
mysql_query(Worker, Q, ParamList) ->
    ParamList2 = pre_encode(ParamList),
	mysql:query(Worker, Q, ParamList2).

pre_encode(List) ->
    lists:map(fun(true) -> 1;
                 (false) -> 0;
                 (undefined) -> null;
                 (X) -> X
              end, List).

maybe_replace_tokens(Q, ParamList) ->
	case sql_bridge_utils:replacement_token() of
		mysql -> {Q, ParamList};
		postgres -> sql_bridge_utils:token_postgres_to_mysql(Q, ParamList)
	end.

format_result(none, _) ->
	ok;
format_result(UID, Count) when UID=:=update;
									 UID=:=insert;
									 UID=:=delete ->
	Count;
format_result(_, ok) ->
	ok;
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
			[list_to_tuple(Row) || Row <- Rows]
	end.

format_lists(Rows) ->
	case sql_bridge_utils:stringify_binaries() of
		true ->
			[format_list(Row) || Row <- Rows];
		false ->
			Rows
	end.

format_list(Row) when is_list(Row) ->
	[normalize_value(V) || V <- Row].

normalize_value(V) when is_tuple(V) ->
    sql_bridge_utils:format_datetime(V);
normalize_value(V) ->
	sql_bridge_stringify:maybe_string(V).

format_proplists(Columns, Rows) ->
	ColNames = extract_colnames(Columns),
	[make_proplist(ColNames, Row) || Row <- Rows].

format_dicts(Columns, Rows) ->
	ColNames = extract_colnames(Columns),
	[make_dict(ColNames, Row) || Row <- Rows].

make_dict(Cols, Row) when is_list(Row) ->
	make_dict(Cols, Row, dict:new()).

make_dict([], [], Dict) ->
	Dict;
make_dict([Col|Cols], [Val|Vals], Dict) ->
	Val2 = normalize_value(Val),
	NewDict = dict:store(Col, Val2, Dict),
	make_dict(Cols, Vals, NewDict).
	
extract_colnames(Columns) ->
	[list_to_atom(binary_to_list(CN)) || CN <- Columns].

make_proplist(Columns, Row) when is_tuple(Row) ->
	make_proplist(Columns, tuple_to_list(Row));
make_proplist([Col|Cols], [Val|Vals]) ->
	Val2 = normalize_value(Val),
	[{Col, Val2} | make_proplist(Cols, Vals)];
make_proplist([], []) ->
	[].

-ifdef(has_maps).
format_maps(Columns, Rows) ->
	ColNames = extract_colnames(Columns),
	[make_map(ColNames, Row) || Row <- Rows].

make_map(Cols, Row) ->
	make_map(Cols, Row, maps:new()).

make_map([], [], Map) ->
	Map;
make_map([Col|Cols],[Val|Vals], Map) ->
	Val2 = normalize_value(Val),
	NewMap = maps:put(Col, Val2, Map),
	make_map(Cols, Vals, NewMap).

-else.
format_maps(_,_) ->
	throw(maps_not_supported).
-endif.

schema_db_column() ->
	"table_schema".

encode(Val) ->
	sql_bridge_utils:with_poolboy_pool(sql_bridge:db(), fun(Worker) ->
		mysql:encode(Worker, Val)
	end).
