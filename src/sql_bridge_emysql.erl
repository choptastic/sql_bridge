-module(sql_bridge_emysql).
-behaviour(sql_bridge_adapter).

-export([
	start/0,
	connect/5,
	query/3,
	query/4,
	schema_db_column/0,
	encode/1
]).

-type sql_result() :: any().
-type json() :: list().


connect(DB, User, Pass, Host, Port) when is_atom(DB) ->
	%% Emysql doesn't use Overflow, so if it maxes out, it's just maxed
	ConnPerPool = sql_bridge_utils:get_env(connections_per_pool, 10),
    emysql:add_pool(DB, ConnPerPool, User, Pass, Host, Port, atom_to_list(DB), utf8),
	ok.

start() ->
	application:start(crypto),
	application:start(emysql),
    ok.

query(Type, Db, Q) ->
	query(Type, Db, Q, []).

query(Type,Db,Q, ParamList) ->
	try query_catched(Type, Db, Q, ParamList)
    catch
        exit:pool_not_found ->
			{error, no_pool}
    end.

query_catched(Type, Db, Q, ParamList) ->
	{Q2, ParamList2} = maybe_replace_tokens(Q, ParamList),	
    NewQ = sql_bridge_utils:q_prep(Q2, ParamList2),
	Res = emysql:execute(Db, NewQ),
	case emysql:result_type(Res) of
		result ->
			{ok, format_result(Type,Res)};
		ok ->
			case Type of
				insert -> {ok, emysql:insert_id(Res)};
				_ ->      {ok, emysql:affected_rows(Res)}
			end;
		error ->
			error_logger:info_msg("Error in SQL: ~s~nRes: ~p~n",[Q, Res]),
			{error, Res}
	end.

maybe_replace_tokens(Q, ParamList) ->
	sql_bridge_mysql_otp:maybe_replace_tokens(Q, ParamList).

-spec format_result(Type :: sql_bridge:return_type(),
					Res :: sql_result()) -> list()
                                            | tuple()
                                            | sql_bridge:t_dict()
                                            | sql_bridge:proplist().
%% @doc Format the results from emysql as a list of Types
format_result(Type,Res) ->
    Json = emysql:as_json(Res),
    case Type of
        list ->
            format_list_result(Json);
        tuple ->
            format_tuple_result(Json);
        proplist ->
            format_proplist_result(Json);
        dict ->
            format_dict_result(Json)
    end.

-spec format_value(V :: term()) -> undefined | string() | any().
%% @doc Stringifies values from the database if the returned value is a binary,
%% otherwise, leaves it be (so numbers are returned as their respective
%% numbers, rather than being converted to strings).
format_value(null) ->
    undefined;
format_value(V) when is_binary(V) ->
    binary_to_list(V);
format_value(V) ->
    V.

-spec format_key(K :: sql_bridge:field()) -> atom().
%% @doc Normalize field values into atoms
format_key(K) when is_atom(K) ->
    K;
format_key(K) when is_binary(K) ->
    format_key(binary_to_list(K));
format_key(K) when is_list(K) ->
    list_to_atom(K).

-spec format_list_result(Json :: sql_bridge:json()) -> [list()].
format_list_result(Json) ->
    [
        [format_value(Value) || {_,Value} <- Row]
    || Row <- Json].

-spec format_tuple_result(Json :: json()) -> [tuple()].
format_tuple_result(Json) ->
    [list_to_tuple(Row) || Row <- format_list_result(Json)].

-spec format_proplist_result(Json :: json()) -> [sql_bridge:proplist()].
format_proplist_result(Json) ->
    [
        [{format_key(F), format_value(V)} || {F,V} <- Row]
    || Row <-Json].

-spec format_dict_result(Json :: json()) -> [sql_bridge:t_dict()].
format_dict_result(Json) ->
    [dict:from_list(PL) || PL <- format_proplist_result(Json)].

schema_db_column() ->
	"table_schema".

-spec encode(V :: any()) -> binary().
%% @doc Safely encodes text for insertion into a query.  Replaces the atoms
%% 'true' and 'false' with <<"1">> and <<"0">> respectively.
encode(true) -> <<"1">>;
encode(false) -> <<"0">>;
encode(L) when is_list(L) -> encode(unicode:characters_to_binary(L));
encode(Other) -> emysql_conn:encode(Other, binary).

