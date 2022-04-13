%% vim: ts=4 sw=4 et
-module(sql_bridge).
-compile(nowarn_export_all).
-compile(export_all).
-include("compat.hrl").

-define(WARNING(QueryText,Msg), error_logger:info_msg("QUERY WARNING: ~p~n~nQuery:~n~p~n~n",[Msg,QueryText])).

-define(ENV(Var, Def), (sql_bridge_utils:get_env(Var, Def))).
-define(ALIAS,  ?ENV(module_alias, db)).
-define(STRINGIFY, ?ENV(stringify_binaries, false)).
-define(ADAPTER,?ENV(adapter, sql_bridge_mysql)).
-define(HOST,   ?ENV(host, "127.0.0.1")).
-define(PORT,   ?ENV(port, undefined)).
-define(USER,   ?ENV(user, "root")).
-define(PASS,   ?ENV(pass, "")).
-define(LOOKUP, ?ENV(lookup, fun() -> throw({sql_bridge,undefined_lookup_method}) end )).
-define(CONNECTION_ATTEMPTS, ?ENV(connection_attempts, 5)).

-define(DB, sql_bridge_cached_db).

-type sql()         :: iolist().
-type db()          :: atom().
-type table()       :: string() | atom().
-type field()       :: string() | atom().
-type value()       :: string() | binary() | atom() | integer() | float().
-type insert_id()   :: term().
-type affected_rows() :: integer().
-type proplist()    :: [{atom(), value()}].
%-type json()        :: list().
-type return_type() :: dict | list | proplist | tuple | insert | update.
-type return_value() :: insert_id() | affected_rows()
                        | [list() | tuple() | t_dict() | proplist()].
-ifdef(has_maps).
-type proplist_or_map() :: tuple() | proplist() | map().
-else.
-type proplist_or_map() :: tuple() | proplist().
-endif.

-export_type([
    sql/0,
    db/0,
    table/0,
    field/0,
    value/0,
    insert_id/0,
    affected_rows/0,
    proplist/0,
    return_type/0,
    return_value/0
]).

%% New API aliases

lists(Q) ->         q(Q).
lists(Q, P) ->      q(Q, P).
list(Q) ->          fr(Q).
list(Q, P) ->       fr(Q, P).
tuples(Q) ->        tq(Q).
tuples(Q, P) ->     tq(Q,P).
tuple(Q) ->         tfr(Q).
tuple(Q, P) ->      tfr(Q, P).
proplists(Q) ->     plq(Q).
proplists(Q, P) ->  plq(Q, P).
proplist(Q) ->      plfr(Q).
proplist(Q, P) ->   plfr(Q, P).
maps(Q) ->          mq(Q).
maps(Q, P) ->       mq(Q, P).
map(Q) ->           mfr(Q).
map(Q, P) ->        mfr(Q, P).
dicts(Q) ->         dq(Q).
dicts(Q, P) ->      dq(Q, P).
dict(Q) ->          dfr(Q).
dict(Q, P) ->       dfr(Q, P).
qupdate(Q) ->       qu(Q).
qupdate(Q, P) ->    qu(Q, P).
qinsert(Q) ->       qi(Q).
qinsert(Q, P) ->    qi(Q, P).
update(Table, Obj) -> plu(Table, Obj).
update(Table, KeyField, Obj) -> plu(Table, KeyField, Obj).
insert(Table, Obj) -> pli(Table, Obj).

-spec lookup() -> db().
% @doc Checks the configuration for how we determine the database we're using
% and returns the database.
lookup() ->
    case ?LOOKUP of
        A when is_atom(A) -> A;
        F when is_function(F, 0) -> F();
        {M, F} when is_atom(M), is_atom(F) -> M:F()
    end.

-spec db() -> db().
% @doc Checks the process dictionary for an active database connection
% associated with this process. If none, then look it up from configuration and
% store it in the process dictionary. Note, this function does not actually
% establish a connection, only returns the name of the database that either is
% associated with the process or that *should* be associated with the process
% based on some criteria (for example, checking host headers)
db() ->
    case erlang:get(?DB) of
        undefined ->
            DB = lookup(),
            db(DB);
        DB ->
            DB
    end.

enable_logging() ->
    error_logger:info_msg("SQL Bridge Started Logging"),
    application:set_env(sql_bridge, logging, true).

disable_logging() ->
    application:set_env(sql_bridge, logging, false),
    error_logger:info_msg("SQL Bridge Stopped Logging").

log(Time, DB, SQL, Params) ->
    Pid = self(),
    Output = io_lib:format("===== ~p (~p): ~p us: ~ts || Params: ~p~n",[Pid, DB, Time, SQL, Params]),
    file:write_file("sql_bridge.log", Output, [append, delayed_write]).

log_for_time(Secs) ->
    spawn(fun() ->
        enable_logging(),
        timer:sleep(Secs*1000),
        disable_logging()
    end).

-spec db(db()) -> db().
% @doc Stores the database name in the process dictionary
db(DB) ->
    erlang:put(?DB,DB),
    DB.

-spec start() -> ok.
% @doc starts the actual database driver, if necessary
start() ->
    {ok, _} = application:ensure_all_started(sql_bridge),
    ok = sql_bridge_alias:build_stringify(?STRINGIFY), 
    ok = sql_bridge_alias:build(?ALIAS),
    ok = ?ADAPTER:start(),
    ok.

-spec connect() -> db().
% @doc establishes a connection to the appropriate database.
connect() ->
    connect(db()).

-spec connect(db()) -> db().
% @doc establishes a connection to the named database.
connect(DB) when is_atom(DB) ->
    Host = case ?HOST of
        {Mod, Fun} ->
            Mod:Fun();
        X ->
            X
    end,
    ok = ?ADAPTER:connect(DB, ?USER, ?PASS, Host, ?PORT),
    DB.

-spec pl(Table :: table(), Data :: proplist_or_map()) -> insert_id().
pl(Table, Data) ->
    save(Table, Data).

-spec pl(Table :: table(), KeyField :: field(), Data :: proplist_or_map()) -> insert_id().
pl(Table, KeyField, Data) ->
    save(Table, KeyField, Data).

-spec save(Table :: table(), Data :: proplist_or_map()) -> insert_id().
save(Table, Data0) ->
    Data = ensure_proplist(Data0),
    save_(Table, Data).

-spec save(Table :: table(), KeyField :: field(), Data0 :: proplist_or_map()) -> insert_id().
save(Table, KeyField, Data0) ->
    Data = ensure_proplist(Data0),
    save_(Table, KeyField, Data).

-spec save_record(Table :: table(), Record :: tuple(), FieldMap :: [atom()]) -> insert_id().
save_record(Table, Record, FieldMap) ->
    PL = sql_bridge_utils:record_to_proplist(Record, FieldMap),
    save(Table, PL).

-spec save_record(Table :: table(), KeyField :: field(), Record :: tuple(), FieldMap :: [atom()]) -> insert_id().
save_record(Table, KeyField, Record, FieldMap) ->
    PL = sql_bridge_utils:record_to_proplist(Record, FieldMap),
    save(Table, KeyField, PL).

save_(Table,PropList) when is_atom(Table) ->
    save_(atom_to_list(Table),PropList);
save_(Table,PropList) when is_list(Table) ->
    KeyField = Table ++ "id",
    save_(Table,KeyField,PropList).

save_(Table,KeyField,PropList) when is_atom(Table) ->
    save_(atom_to_list(Table),KeyField,PropList);
save_(Table,KeyField,PropList) when is_list(KeyField) ->
    save_(Table,list_to_atom(KeyField),PropList);
save_(Table,KeyField,PropList) when is_list(Table) ->
    KeyValue = proplists:get_value(KeyField,PropList,0),
    case KeyValue of
        Zero when Zero == 0;
                  Zero == "0";
                  Zero == undefined;
                  Zero == null;
                  Zero == "";
                  Zero == <<>> -> 
            pli(Table,lists:keydelete(KeyField,1,PropList));
        _ -> 
            plu(Table,KeyField,PropList)
    end.


-ifdef(has_maps).
ensure_proplist(Record) when is_atom(element(1, Record)) ->
    ensure_proplist(sql_bridge_utils:convert_record(Record));
ensure_proplist(Map) when is_map(Map) ->
    maps:to_list(Map);
ensure_proplist(PL) when is_list(PL) ->
    PL.
-else.
ensure_proplist(Record) when is_atom(element(1, Record)) ->
    ensure_proplist(sql_bridge_utils:convert_record(Record));
ensure_proplist(PL) when is_list(PL) ->
    PL.
-endif.

-spec filter_fields(Table :: table(), PropList :: proplist()) -> proplist().
% @doc removes from Proplist any fields that aren't found in the table "Table"
filter_fields(Table,PropList) ->
    TableFields = table_fields(Table),
    [{K,V} || {K,V} <- PropList,lists:member(sql_bridge_utils:to_atom(K),TableFields)].

-spec trans(Fun :: fun()) -> ok.
trans(Fun) when is_function(Fun) ->
    ?ADAPTER:with_transaction(db(), Fun).

-spec start_trans() -> ok.
start_trans() ->
    ?ADAPTER:start_transaction(db()).

-spec commit() -> ok.
commit() ->
    ?ADAPTER:commit_transaction(db()).

rollback() ->
    ?ADAPTER:rollback_transaction(db()).

-spec q(Q :: sql()) -> [list()].
%% @doc Run the SQL query `Q` and return a list of lists, with each inner list
%% representing one record in the return set.
q(Q) ->
    Db = db(),
    db_q(list,Db,Q).

-spec q(Q :: sql(), ParamList :: [value()]) -> [list()].
%% @doc Run the SQL query `Q`, with the the values of `ParamList` safely
%% injected into the query replacing any instances of question marks (`?`),
%% with each inner list representing one record in the return set (same as q/1).
q(Q,ParamList) ->
    Db = db(),
    db_q(list,Db,Q,ParamList).

-spec dq(Q :: sql()) -> [t_dict()].
%% @doc Same as q/1, but returns a list of dicts.
dq(Q) ->
    Db = db(),
    db_q(dict,Db,Q).

-spec dq(Q :: sql(), ParamList :: [value()]) -> [t_dict()].
%% @doc Same as q/2, but returns a list of dicts
dq(Q,ParamList) ->
    Db = db(),
    db_q(dict,Db,Q,ParamList).

-ifdef(has_maps).
-spec mq(Q :: sql()) -> [map()].
%% @doc Same as d1/, but returns a list of maps
mq(Q) ->
    Db = db(),
    db_q(map, Db, Q).

-spec mq(Q :: sql(), ParamList :: [value()]) -> [map()].
%% @doc Same as d1/, but returns a list of maps
mq(Q, ParamList) ->
    Db = db(),
    db_q(map, Db, Q, ParamList).
-endif.

-spec tq(Q :: sql()) -> [tuple()].
%% @doc Same as q/1, but returns a list of tuples
tq(Q) ->
    Db = db(),
    db_q(tuple,Db,Q).

-spec tq(Q :: sql(), ParamList :: [value()]) -> [tuple()].
%% @doc Same as q/2, but returns a list of tuples
tq(Q,ParamList) ->
    Db = db(),
    db_q(tuple,Db,Q,ParamList).

-spec plq(Q :: sql()) -> [proplist()].
%% @doc Same as q/1, but returns a list of proplists
plq(Q) ->
    Db = db(), db_q(proplist,Db,Q).

-spec plq(Q :: sql(), ParamList :: [value()]) -> [proplist()].
%% @doc Same as q/2, but returns a list of proplists
plq(Q,ParamList) ->
    Db = db(),
    db_q(proplist,Db,Q,ParamList).


-spec pli(Table :: table(), Data :: proplist_or_map()) -> insert_id().
%% @doc Inserts a proplist into the table
pli(Table,PropList) when is_atom(Table) ->
    pli(atom_to_list(Table),PropList);
pli(Table,InitPropList0) ->
    InitPropList = ensure_proplist(InitPropList0),
    PropList = filter_fields(Table,InitPropList),
    Fields0 = [wrap_field(F) || {F,_} <- PropList],
    Fields = iolist_join(Fields0, ","),
    Values = [V || {_,V} <- PropList],
    Placeholders = sql_bridge_utils:create_placeholders(length(Values)),
    PlaceholderString = iolist_join(Placeholders, ","),
    SQL = ["insert into ",Table,"(",Fields,") values(",PlaceholderString,");"],
    qi(SQL, Values).

-spec plu(Table :: table(), PropList :: proplist_or_map()) -> affected_rows().
%% @doc Updates a row from the proplist based on the key `Table ++ "id"` in the Table
plu(Table,PropList) when is_atom(Table) ->
    plu(atom_to_list(Table),PropList);
plu(Table,PropList0) ->
    PropList = ensure_proplist(PropList0),
    KeyField = list_to_atom(Table ++ "id"),
    plu(Table,KeyField,PropList).

-spec plu(Table :: table(), KeyField :: field(), PropList :: proplist()) -> affected_rows().
%% @doc Update a row from proplist based on the Keyfield `Keyfield` on provided Table
plu(Table,KeyField,InitPropList) when is_atom(Table) ->
    plu(atom_to_list(Table),KeyField,InitPropList);
plu(Table,KeyField,InitPropList) ->
    PropList = filter_fields(Table,InitPropList),
   
    SetFields = [wrap_field(F) || {F, _} <- PropList, F =/= KeyField],
    SetPlaceholders = sql_bridge_utils:create_placeholders(length(SetFields)),
    Sets = [ [F,"=",PH] || {F, PH} <- lists:zip(SetFields, SetPlaceholders) ],
    Set = iolist_join(Sets,","),
   
    SetValues = [V || {F, V} <- PropList, F =/= KeyField],
    KeyValue = proplists:get_value(KeyField,PropList),
    KeyPlaceholder = sql_bridge_utils:create_placeholder(length(SetFields)+1),
    Params = SetValues ++ [KeyValue],

    SQL = ["update ",Table," set ",Set," where ",atom_to_list(KeyField),"=",KeyPlaceholder],
    q(SQL, Params),
    KeyValue.

-spec db_q(Type :: return_type(), Db :: db(), Q :: sql()) ->  insert_id() 
                                                            | affected_rows()
                                                            | [list() | t_dict() | tuple() | proplist()].
%% @doc Query from the specified Database pool (Db) This will connect to the
%% specified Database Pool Type must be atoms: proplist, dict, list, or tuple
%% Type can also be atom 'insert' in which case, it'll return the insert value
db_q(Type,Db,Q) ->
    db_q(Type, Db, Q, []).

-spec db_q(Type :: return_type(), Db :: db(),
           Q :: sql(), ParamList :: [value()]) ->   insert_id() 
                                                  | affected_rows()
                                                  | [list() | t_dict() | tuple() | proplist()].
%% @doc Same as db_q/3, but ParamList is safely inserted into the Query
db_q(Type,Db,Q,ParamList) ->
    ParamList2 = sanitize_params(ParamList),
    db_q(Type, Db, Q, ParamList2, ?CONNECTION_ATTEMPTS).


db_q(_Type, Db, _Q, _ParamList, _RemainingAttempts=0) ->
    error_logger:error_msg("Unable to connect to pool '~p' after ~p attempts", [Db, ?CONNECTION_ATTEMPTS]),
    throw({error, unable_to_connect_to_pool, Db});
db_q(Type, Db, Q, ParamList, RemainingAttempts) ->
    {Time, Return} = timer:tc(fun() ->
        case ?ADAPTER:query(Type, Db, Q, ParamList) of
            {ok, Response} ->
                Response;
            {error, no_pool} ->
                connect(Db),
                db_q(Type, Db, Q, ParamList, RemainingAttempts-1);
            {error, disconnected} ->
                error_logger:warning_msg("WARN: Disconnected worker in pool: ~p~n",[Db]),
                db_q(Type, Db, Q, ParamList, RemainingAttempts-1);
            {error, Other} ->
                error_logger:error_msg("Error in SQL Statement or Adapter:~nDB: ~p~nSQL: ~p~nParams: ~p~nError Message: ~p",[Db, Q, ParamList, Other]),
                exit(unknown_error)
        end
    end),
    case application:get_env(sql_bridge, logging, false) of
        false -> ok;
        true -> log(Time, Db, Q, ParamList)
    end,
    Return.

-spec qi(Q :: sql()) -> insert_id().
%% @doc A special Query function just for inserting.
%% Inserts the record(s) and returns the insert_id
qi(Q) ->
    Db = db(),
    db_q(insert,Db,Q).

-spec qi(Q :: sql(), ParamList :: [value()]) -> insert_id().
qi(Q,ParamList) ->
    Db = db(),
    db_q(insert,Db,Q,ParamList).

-spec qu(Q :: sql()) -> affected_rows().
qu(Q) ->
    Db = db(),
    db_q(update, Db, Q).

-spec qu(Q :: sql(), ParamList :: [value()]) -> affected_rows().
qu(Q, ParamList) ->
    Db = db(),
    db_q(update, Db, Q, ParamList).

-spec plfr(Q :: sql(), ParamList :: [value()]) -> proplist() | not_found.
%% @doc fr = First Record
plfr(Q,ParamList) ->
    case plq(Q,ParamList) of
        [] -> not_found;
        [First|_] -> First
    end.

-spec plfr(Q :: sql()) -> proplist() | not_found.
plfr(Q) ->
    plfr(Q,[]).

-ifdef(has_maps).

-spec mfr(Q :: sql(), ParamList :: [value()]) -> map() | not_found.
%% @doc fr = First Record
mfr(Q,ParamList) ->
    case mq(Q,ParamList) of
        [] -> not_found;
        [First|_] -> First
    end.

-spec mfr(Q :: sql()) -> map() | not_found.
mfr(Q) ->
    mfr(Q,[]).

-endif.

-spec tfr(Q :: sql()) -> tuple() | not_found.
tfr(Q) ->
    tfr(Q,[]).

-spec tfr(Q :: sql(), ParamList :: [value()]) -> tuple() | not_found.
tfr(Q,ParamList) ->
    case tq(Q,ParamList) of
        [] -> not_found;
        [First|_] -> First
    end.

-spec dfr(Q :: sql()) -> t_dict() | not_found.
dfr(Q) ->
    dfr(Q, []).

-spec dfr(Q :: sql(), ParamList :: [value()]) -> t_dict() | not_found.
dfr(Q, ParamList) ->
    case dq(Q, ParamList) of
        [] -> not_found;
        [First|_] -> First
    end.

%% fr = First Record
-spec fr(Q :: sql(), ParamList :: [value()]) -> list() | not_found.
fr(Q,ParamList) ->
    case q(Q,ParamList) of
        [] -> not_found;
        [First|_] -> First
    end.

-spec fr(Q :: sql()) -> list() | not_found.
fr(Q) ->
    fr(Q,[]).

-spec fffr(Q :: sql(), ParamList :: [value()]) -> string() | integer() | not_found.
%% @doc Get First Field of First record
fffr(Q,ParamList) ->
    case fr(Q,ParamList) of
        not_found -> not_found;
        [First|_] -> First
    end.

-spec fffr(Q :: sql()) -> string() | integer() | not_found.
fffr(Q) ->
    fffr(Q,[]).

%% First Field List
-spec ffl(Q :: sql(), ParamList :: [value()]) -> [string() | integer()].
ffl(Q,ParamList) ->
    [First || [First | _ ] <- q(Q,ParamList)].

-spec ffl(Q :: sql()) -> [string() | integer()].
ffl(Q) ->
    ffl(Q,[]).

-spec table_fields(Table :: table()) -> [atom()].
%% deprecate this. Use "fields"
table_fields(Table) when is_atom(Table) ->
    table_fields(atom_to_list(Table));
table_fields(Table0) ->
    {DB, Table} = table_and_db(Table0),
    [T1, T2] = sql_bridge_utils:create_placeholders(2),
    DBCol = ?ADAPTER:schema_db_column(),
    SQL = [<<"select column_name
             from information_schema.columns
             where ">>,DBCol,<<"=">>,T1,<<" and table_name=">>,T2],
    [sql_bridge_utils:to_atom(F) || F <- ffl(SQL, [DB, Table])].

table_and_db(Table) ->
    case string:tokens(Table, ".") of
        [DB, TableOnly] ->
            {DB, TableOnly};
        [TableOnly] ->
            {db(), TableOnly}
    end.

-spec fields(Table :: table()) -> [atom()].
fields(Table) ->
    table_fields(Table).

field_exists(Table0, Field) ->
    {DB, Table} = table_and_db(Table0),
    [T1, T2, T3] = sql_bridge_utils:create_placeholders(3),
    DBCol = ?ADAPTER:schema_db_column(),
    SQL = [<<"select column_name
             from information_schema.columns
             where ">>,DBCol,<<"=">>,T1,<<" and table_name=">>,T2,<<" and column_name=">>,T3],
    qexists(SQL, [DB, Table, Field]).

-spec qexists(Q :: sql()) -> boolean().
%% @doc Existance query, just returns true if the query Q returns anything
%% other than an empty set.
%% TODO: Check for "limit" clause and add? Or rely on user.
qexists(Q) ->
    qexists(Q,[]).

-spec qexists(Q :: sql(), ParamList :: [value()]) -> boolean().
qexists(Q,ParamList) ->
    case q(Q,ParamList) of
        [] -> false;
        [_] -> true;
        [_|_] -> 
            ?WARNING({Q,ParamList},"qexists returned more than one record. Recommend returning one record for performance."),
            true
    end.

-spec exists(Table :: table(), IDValue :: value()) -> boolean().
%% @doc Returns true if Table has a record representing Key Value IDValue
exists(Table, IDValue) when is_atom(Table) ->
    exists(atom_to_list(Table), IDValue);
exists(Table, IDValue) when is_list(Table) ->
    exists(Table, Table ++ "id", IDValue).

-spec exists(Table :: table(), KeyField :: field(), IDValue :: value()) -> boolean().
%% @doc Returns true if Table has a record where KeyField = IDValue
exists(Table, KeyField, IDValue) ->
    case field(Table, KeyField, KeyField, IDValue) of
        not_found -> false;
        _ -> true
    end.

-spec field(Table :: table(), Field :: field(), IDField :: field(), IDValue :: value()) -> value() | not_found.
%% @doc retrieves the value of Field from Table where the value of IDField ==
%% IDValue (e.g.: Select 'Field' from 'Table' where 'IDField'='IDValue'). If
%% the query returns more than one record, only the first record's value is
%% returned.
field(Table,Field,IDField,IDValue) when is_atom(Table) ->
    field(atom_to_list(Table),Field,IDField,IDValue);
field(Table,Field,IDField,IDValue) when is_atom(Field) ->
    field(Table,atom_to_list(Field),IDField,IDValue);
field(Table,Field,IDField,IDValue) when is_atom(IDField) ->
    field(Table,Field,atom_to_list(IDField),IDValue);
field(Table,Field,IDField,IDValue) ->
    Token = sql_bridge_utils:create_placeholder(1),
    fffr(["select ",Field," from ",Table," where ",IDField,"= ",Token],[IDValue]).

-spec field(Table :: table(), Field :: field(), Value :: value()) -> value() | not_found.
%% @doc This does the same as above, but uses Table ++ "id" for the idfield
field(Table,Field,IDValue) when is_atom(Table) ->
    field(atom_to_list(Table),Field,IDValue);
field(Table,Field,IDValue) ->
    field(Table,Field,Table ++ "id",IDValue).

-spec delete(Table :: table(), ID :: value()) -> affected_rows().
%% @doc Deletes rows from Table where the Table ++ "id" = ID
delete(Table,ID) when is_atom(Table) ->
    delete(atom_to_list(Table),ID);
delete(Table,ID) when is_list(Table) ->
    KeyField = Table ++ "id",
    delete(Table,KeyField,ID).

-spec delete(Table :: table(), KeyField :: field(), ID :: value()) -> affected_rows().
%% @doc Deletes from Table where KeyField = ID
delete(Table,KeyField,ID) when is_atom(Table) ->
    delete(atom_to_list(Table),KeyField,ID);
delete(Table,KeyField,ID) when is_atom(KeyField) ->
    delete(Table,atom_to_list(KeyField),ID);
delete(Table,KeyField,ID) ->
    Token = sql_bridge_utils:create_placeholder(1),
    qu(["delete from ",Table," where ",KeyField,"=",Token],[ID]).

sanitize(V) when is_list(V) ->
    sanitize(unicode:characters_to_binary(V));
sanitize(true) ->
    true;
sanitize(false) ->
    false;
sanitize(undefined) ->
    undefined;
sanitize(A) when is_atom(A) ->
    sanitize(atom_to_list(A));
sanitize(V) ->
    V.

sanitize_params(L) ->
    [sanitize(X) || X <- L].


-spec encode(V :: any()) -> binary().
%% @doc Safely encodes text for insertion into a query.  Replaces the atoms
%% 'true' and 'false' with <<"1">> and <<"0">> respectively.
encode(Other) ->
    ?ADAPTER:encode(sanitize(Other)).

remove_wrapping_quotes(Bin) when is_binary(Bin) ->
    binary_part(Bin, 1, byte_size(Bin)-2);
remove_wrapping_quotes(Str) ->
    lists:reverse(tl(lists:reverse(tl(Str)))).

-spec encode64(T :: any()) -> string().
%% @doc Encodes an erlang term into a base64 string which can be safely
%% inserted into a text field 
encode64("") -> "";
encode64(undefined) -> "";
encode64(Data) ->
    binary_to_list(b64fast:encode64(term_to_binary(Data))).
    %base64:encode_to_string(term_to_binary(Data)).

-spec decode64(T :: any()) -> string().
%% @doc Decodes a base64 string into the relevant erlang term.
decode64("") -> "";
decode64(undefined) -> "";
decode64(Data) when is_list(Data) ->
    decode64(list_to_binary(Data));
decode64(Data) when is_binary(Data) ->
    binary_to_term(b64fast:decode64(Data)).

-spec encode_list(List :: [value()]) -> iolist().
%% @doc Takes a list of items and encodes them for SQL then returns a
%% comma-separated list of them.
encode_list(List) ->
    NewList = [encode(X) || X<-List],
    iolist_join(NewList,",").

-spec dict_to_proplist(SrcDict :: t_dict(), AcceptableFields :: [field()]) -> proplist().
%% @doc Converts a dict to a proplist, filtering out any fields not found in
%% AcceptableFields
dict_to_proplist(SrcDict,AcceptableFields) ->
    DictFilterFoldFun = fun(F,Dict) ->
        case dict:is_key(F,Dict) of
            true -> Dict;
            false -> dict:erase(F,Dict)
        end
    end,
    FilteredDict = lists:foldl(DictFilterFoldFun,SrcDict,AcceptableFields),
    dict:to_list(FilteredDict).

-spec iolist_join(List :: [iolist()], Delimiter :: iolist()) -> iolist().
%% @doc Joins a list of iolists together by a delimiter
iolist_join([], _) ->
    [];
iolist_join([H], _) ->
    [H];
iolist_join([H|T], Delimiter) ->
    [H,Delimiter|iolist_join(T, Delimiter)].

to_bool(false) ->   false;
to_bool(0) ->       false;
to_bool(undefined) ->   false;
to_bool(_) ->       true.

-spec limit_clause(PerPage :: integer(), Page :: integer()) -> iolist().
%% @doc Generates a SQL "LIMIT" clause based on the PerPage Criteria and the
%% Page, calculating the offset according to the provided values. Useful when
%% building something that breaks a list into pages in an interface.
limit_clause(PerPage, Page) when PerPage < 1 ->
    limit_clause(1, Page);
limit_clause(PerPage, Page) ->
    Offset = offset(PerPage, Page),
    [" limit ",integer_to_list(PerPage)," offset ", integer_to_list(Offset)].

-spec offset(PerPage :: integer(), Page :: integer()) -> Offset :: integer().
offset(PerPage, Page) when Page =< 0 ->
    offset(PerPage, 1);
offset(PerPage, Page) when PerPage < 1 ->
    offset(1, Page);
offset(PerPage, Page) when Page > 0 ->
    (Page-1) * PerPage.

%% Exported as a convenience
q_prep(Q, ParamList) ->
    sql_bridge_utils:q_prep(Q, ParamList).


wrap_field(V) ->
    ?ADAPTER:wrap_field(V).
