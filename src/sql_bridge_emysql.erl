-module(sql_bridge_emysql).
-behaviour(sql_bridge_adapter).
-include_lib("emysql/include/emysql.hrl").
-define(IS_DATE(Type), Type==?FIELD_TYPE_DATETIME orelse Type==?FIELD_TYPE_TIMESTAMP).

-export([
	start/0,
	connect/5,
	query/3,
	query/4,
	schema_db_column/0,
	encode/1,
    wrap_field/1
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

wrap_field(V) ->
    "`" ++ sql_bridge_utils:to_string(V) ++ "`".

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
    Fields = get_field_types(Res),
    DateFields = [?IS_DATE(F) || F <- Fields],
	Stringify = sql_bridge_utils:stringify_binaries(),
    case Type of
        list ->
            format_list_result(DateFields, Stringify, Json);
        tuple ->
            format_tuple_result(DateFields, Stringify, Json);
        proplist ->
            format_proplist_result(DateFields, Stringify, Json);
        dict ->
            format_dict_result(DateFields, Stringify, Json);
		map ->
			format_map_results(DateFields, Stringify, Json)
    end.

get_field_types(#result_packet{field_list=Fs}) ->
    [F#field.type || F <- Fs].

-spec format_key(K :: sql_bridge:field()) -> atom().
%% @doc Normalize field values into atoms
format_key(K) when is_atom(K) ->
    K;
format_key(K) when is_binary(K) ->
    format_key(binary_to_list(K));
format_key(K) when is_list(K) ->
    list_to_atom(K).

-spec format_list_result(DateFields :: [boolean()], Stringify :: boolean(), Json :: sql_bridge:json()) -> [list()].
format_list_result(DateFields, Stringify, Json) ->
    [
        [format_value(IsDate, Stringify, Value) || {IsDate, {_,Value}} <- lists:zip(DateFields, Row)]
    || Row <- Json].

-spec format_tuple_result(DateFields :: [boolean()], Stringify :: boolean(), Json :: json()) -> [tuple()].
format_tuple_result(DateFields, Stringify, Json) ->
    [list_to_tuple(Row) || Row <- format_list_result(DateFields, Stringify, Json)].

-spec format_proplist_result(DateFields :: [boolean()], Stringify :: boolean(), Json :: json()) -> [sql_bridge:proplist()].
format_proplist_result(DateFields, Stringify, Json) ->
    [
        [{format_key(F), format_value(IsDate, Stringify, V)} || {IsDate, {F,V}} <- lists:zip(DateFields, Row)]
    || Row <-Json].

-spec format_dict_result(DateFields :: [boolean()], Stringify :: boolean(), Json :: json()) -> [sql_bridge:t_dict()].
format_dict_result(DateFields, Stringify, Json) ->
    [dict:from_list(PL) || PL <- format_proplist_result(DateFields, Stringify, Json)].

-spec format_map_results(DateFields :: [boolean()], Stringify :: boolean(), Json :: json()) -> [map()].
format_map_results(DateFields, Stringify, Json) ->
    [maps:from_list(PL) || PL <- format_proplist_result(DateFields, Stringify, Json)].

schema_db_column() ->
	"table_schema".

format_value(_, _, null) ->
	undefined;
format_value(true, Stringify, V) ->
    Return = case Stringify of
        true -> list;
        false -> binary
    end,
    {match, [D,T]} = re:run(V, "^(\\d{4}-\\d{2}-\\d{2})T(\\d{2}:\\d{2}:\\d{2})Z$",[{capture, all_but_first, Return}]),
    case Stringify of
        true -> D ++ " " ++ T;
        false -> <<D/binary," ",T/binary>>
    end;
format_value(_, true, V) when is_binary(V) ->
	sql_bridge_utils:binary_to_string(V);
format_value(_, _, V) when is_tuple(V) ->
    sql_bridge_utils:format_datetime(V);
format_value(_, _, V) ->
    V.

-spec encode(V :: any()) -> binary().
%% @doc Safely encodes text for insertion into a query.  Replaces the atoms
%% 'true' and 'false' with <<"1">> and <<"0">> respectively.
encode(true) -> <<"1">>;
encode(false) -> <<"0">>;
encode(Other) -> emysql_conn:encode(Other, binary).

