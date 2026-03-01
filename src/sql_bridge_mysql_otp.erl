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
         with_transaction/2,
         wrap_field/1,
         primary_key/2,
         is_auto_increment/3,
         %primary_key_with_auto_increment_and_type/2,
         %auto_increment/2,
         field_type/3,
         maybe_replace_tokens/2
        ]).

%-export([maybe_replace_tokens/2]).


start() ->
    application:start(poolboy),
    ok.

connect(DB, User, Pass, Host, Port) when is_atom(DB) ->
    WorkerArgs = [
        {database, atom_to_list(DB)},
        {host, Host},
        {user, User},
        {password, Pass},
        {port, Port},
        %% This is an option for mysql-otp. It is probably worth adding it as a tweakable option for sql_bridge
        {decode_decimal, number}
    ],
    sql_bridge_utils:start_poolboy_pool(DB, WorkerArgs, mysql),
    ok.

wrap_field(V) ->
    "`" ++ sql_bridge_utils:to_string(V) ++ "`".

start_transaction(DB) ->
    case sql_bridge_utils:trans_depth(DB) of
        0 ->
sql_bridge_utils:checkout_pool(DB),
            query(none, DB, "BEGIN", []);
        _ ->
            ok
    end,
    sql_bridge_utils:trans_deeper(DB),
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
        Error:Class:ST ->
            ErrMsg = [{Error, Class}, ST],
            logger:warning("Errored Query: ~p",[ErrMsg]),
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
            logger:error("Unhandled Error: ~p:~p", [E,T]),
            throw(unhandled_error)
    end.

query_catched(Type, DB, Q, ParamList) ->
    {Q2, ParamList2} = maybe_replace_tokens(Q, ParamList),
    ToRun = fun(Worker) ->
        Res = mysql_query(Worker, Q2, ParamList2),
        case Res of
            {error, Reason} ->
                logger:warning("Error in Query.~nError: ~p~nQuery: ~s",[Reason, Q]);
            _ ->
                ok
        end,
        %% NOTE: For now, not going to match on the {error,_} tuple just to see
        %% the impact it might have on my libraries before doing this.
        case {Res, Type} of
            %{{error, _}, _} -> Res;
            {_, insert} -> mysql:insert_id(Worker);
            {_, update} -> mysql:affected_rows(Worker);
            _ -> Res
        end
    end,
    case sql_bridge_utils:with_poolboy_pool(DB, ToRun) of
        {error, Reason} ->
            {error, Reason};
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

schema_db_column() ->
    "table_schema".

encode(Val) ->
    sql_bridge_utils:with_poolboy_pool(sql_bridge:db(), fun(Worker) ->
        mysql:encode(Worker, Val)
    end).

%primary_key_with_auto_increment_and_type(DB, Table) ->
%    [T1, T2] = sql_bridge_utils:create_placeholders(2),
%    SQL = [<<"select column_name
%            from information_schema.columns
%            where table_schema=">>,T1,
%                <<" and table_name=">>,T2,
%                <<" and column_key='PRI'
%                and extra like '%auto_increment%'">>],
%    Res = sql_bridge:list(SQL, [DB, Table]),
%    %io:format("Res: ~p~n", [Res]),
%    case Res of 
%        [Field] -> list_to_atom(Field);
%        _ -> undefined
%    end.

%%auto_increment(DB, Table) ->
%%    Field = primary_key(DB, Table),
%%    io:format("Primary Key: ~p -> ~p~n", [Table, Field]),
%%    ID = case get_field_type(DB, Table, Field) of
%%        {uuid, _} ->
%%            auto_increment_uuid();
%%        {text, MaxLength} ->
%%            auto_increment_string(MaxLength);
%%        {integer, {Min, Max}} ->
%%            auto_increment_integer(Min, Max)
%%    end,
%%    io:format("Auto-Generated ID: ~p~n",[ID]),
%%    FullTable = sql_bridge_utils:to_string(DB) ++ "." ++ sql_bridge_utils:to_string(Table),
%%    case sql_bridge:exists(FullTable, Field, ID) of
%%        true -> auto_increment(DB, Table);
%%        false -> ID
%%    end.
%%
%%auto_increment_uuid() ->
%%    uuid:uuid_to_string(uuid:get_v4_urandom()).
%%
%%auto_increment_string(MaxLength) ->
%%    [rand_char() || _ <- lists:seq(1, MaxLength)].
%%
%%rand_char() ->
%%    case rand:uniform(62) of
%%        10 -> $0;
%%        X when X < 10 ->
%%            X + $0;
%%        X when X =< 36 ->
%%            X - 11 + $a;
%%        X ->
%%            X - 37 + $A
%%    end.
%%
%%auto_increment_integer(Min, Max) ->
%%    Diff = Max - Min,
%%    rand:uniform(Diff) + Min.

-spec field_type(DB :: sql_bridge:db(),
                     Table :: sql_bridge:table(),
                     Field :: sql_bridge:field()) -> undefined | sql_bridge:field_type().

field_type(DB, Table, Field) ->
    [T1, T2, T3] = sql_bridge_utils:create_placeholders(3),
    SQL = [<<"select column_type from information_schema.columns ">>,
           <<" where table_schema=">>, T1,
           <<" and table_name=">>, T2,
           <<" and column_name=">>, T3],
    Res = sql_bridge:fffr(SQL, [DB, Table, Field]),
    io:format("MySQL - Field Type (~p, ~p, ~p): ~p~n", [DB, Table, Field, Res]),
    case Res of
        not_found ->
            undefined;
        Type ->
            parse_field_type(Type)
    end.

parse_field_type(T) when T=="text";
                         T=="mediumtext";
                         T=="longtext";
                         T=="blob";
                         T=="mediumblob";
                         T=="longblob" ->
    {text, 50};
parse_field_type("uuid") ->
    {uuid, undefined};
parse_field_type(FieldType) ->
    RETypes = "(tinyint|smallint|mediumint|int|bigint|varchar|char)",
    RELength = "\\(([0-9]+)\\)",
    REFollower = "(.*?)",
    RE = "^" ++ RETypes ++ "\\s*" ++ RELength ++ "\\s*" ++ REFollower ++ "$",
    case re:run(FieldType, RE, [{capture, all_but_first, binary}]) of
        {match, [Type, Length | _]} when Type == <<"varchar">>;
                                         Type == <<"char">> ->
            {text, binary_to_integer(Length)};
        {match, [Type, _Size]} ->
            Range = int_range({Type, <<"signed">>}),
            {integer, Range};
        {match, [Type, _Size, Unsigned]} ->
            Range = int_range({Type, Unsigned}),
            {integer, Range};
        nomatch ->
            undefined
    end.

int_range(TypeUnsigned) ->
    AType = case TypeUnsigned of
        {<<"tinyint">>, <<"unsigned">>} -> uint1;
        {<<"smallint">>, <<"unsigned">>} -> uint2;
        {<<"mediumint">>, <<"unsigned">>} -> uint3;
        {<<"int">>, <<"unsigned">>} -> uint4;
        {<<"bigint">>, <<"unsigned">>} -> uint8;

        {<<"tinyint">>, _} -> int1;
        {<<"smallint">>, _} -> int2;
        {<<"mediumint">>, _} -> int3;
        {<<"int">>, _} -> int4;
        {<<"bigint">>, _} -> int8
    end,
    sql_bridge_utils:int_ranges(AType).


primary_key(DB, Table) ->
    [T1, T2] = sql_bridge_utils:create_placeholders(2),
    SQL = [<<"select column_name
            from information_schema.columns
            where table_schema=">>, T1,
                <<" and table_name=">>, T2,
                <<" and column_key='PRI'">>],
    case sql_bridge:fffr(SQL, [DB, Table]) of
        not_found ->
            undefined;
        Field ->
            list_to_atom(Field)
    end.

is_auto_increment(DB, Table, Field) ->
    [T1, T2, T3] = sql_bridge_utils:create_placeholders(3),
    SQL = "select column_name
            from information_schema.columns
            where table_schema=" ++ T1 ++ "
                and table_name=" ++ T2 ++ "
                and column_name=" ++ T3 ++ "
                and extra like '%auto_increment%'",
    case sql_bridge:fffr(SQL, [DB, Table, Field]) of
        not_found ->
            false;
        _ ->
            true
    end.
