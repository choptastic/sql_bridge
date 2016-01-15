%% vim: ts=4 sw=4 et
-module(sql_bridge).
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

-spec db(db()) -> db().
% @doc Stores the database name in the process dictionary
db(DB) ->
    erlang:put(?DB,DB),
    DB.

-spec start() -> ok.
% @doc starts the actual database driver, if necessary
start() ->
    application:start(sql_bridge),
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
    ok = ?ADAPTER:connect(DB, ?USER, ?PASS, ?HOST, ?PORT),
    DB.

-spec pl(Table :: table(), Proplist :: proplist()) -> insert_id() | affected_rows().
% @doc Shortcut for pl(Table, Table ++ "id", PropList)
pl(Table,PropList) when is_atom(Table) ->
    pl(atom_to_list(Table),PropList);
pl(Table,PropList) when is_list(Table) ->
    KeyField = Table ++ "id",
    pl(Table,KeyField,PropList).

-spec pl(Table :: table(), KeyField :: field(), PropList :: proplist()) -> insert_id() | affected_rows().
% @doc Inserts into or updates a table based on the value of the KeyField i1n
% PropList. If get_value(KeyField, PropList) is "0", 0, or undefined, then
% insert, otherwise update
pl(Table,KeyField,PropList) when is_atom(Table) ->
    pl(atom_to_list(Table),KeyField,PropList);
pl(Table,KeyField,PropList) when is_list(KeyField) ->
    pl(Table,list_to_atom(KeyField),PropList);
pl(Table,KeyField,PropList) when is_list(Table) ->
    KeyValue = proplists:get_value(KeyField,PropList,0),
    case KeyValue of
        Zero when Zero == 0;
                  Zero == "0";
                  Zero == undefined;
                  Zero == "";
                  Zero == <<>> -> 
            pli(Table,lists:keydelete(KeyField,1,PropList));
        _ -> 
            plu(Table,KeyField,PropList)
    end.

-spec atomize(list() | binary() | atom()) -> atom().
% @doc converts X to an atom.
atomize(X) when is_list(X) ->
    list_to_atom(X);
atomize(X) when is_atom(X) ->
    X;
atomize(X) when is_binary(X) ->
    list_to_atom(binary_to_list(X)).

-spec filter_fields(Table :: table(), PropList :: proplist()) -> proplist().
% @doc removes from Proplist any fields that aren't found in the table "Table"
filter_fields(Table,PropList) ->
    TableFields = table_fields(Table),
    [{K,V} || {K,V} <- PropList,lists:member(atomize(K),TableFields)].

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


%% TODO: This relies on mysql's insert...set syntax, which isn't valid.
-spec pli(Table :: table(), PropList :: proplist()) -> insert_id().
%% @doc Inserts a proplist into the table
pli(Table,PropList) when is_atom(Table) ->
    pli(atom_to_list(Table),PropList);
pli(Table,InitPropList) ->
    PropList = filter_fields(Table,InitPropList),
    Fields0 = [atom_to_list(F) || {F,_} <- PropList],
    Fields = iolist_join(Fields0, ","),
    Values = [V || {_,V} <- PropList],
    Placeholders = iolist_join(lists:duplicate(length(Values), "?"), ","),
    SQL = ["insert into ",Table,"(",Fields,") values(",Placeholders,");"],
    qi(SQL, Values).

-spec plu(Table :: table(), PropList :: proplist()) -> affected_rows().
%% @doc Updates a row from the proplist based on the key `Table ++ "id"` in the Table
plu(Table,PropList) when is_atom(Table) ->
    plu(atom_to_list(Table),PropList);
plu(Table,PropList) ->
    KeyField = list_to_atom(Table ++ "id"),
    plu(Table,KeyField,PropList).

-spec plu(Table :: table(), KeyField :: field(), PropList :: proplist()) -> affected_rows().
%% @doc Update a row from proplist based on the Keyfield `Keyfield` on provided Table
plu(Table,KeyField,InitPropList) when is_atom(Table) ->
    plu(atom_to_list(Table),KeyField,InitPropList);
plu(Table,KeyField,InitPropList) ->
    PropList = filter_fields(Table,InitPropList),
    KeyValue = proplists:get_value(KeyField,PropList),
    Sets = [ [atom_to_list(F),"=",encode(V)] || {F,V} <- PropList,F /= KeyField],
    Set = iolist_join(Sets,","),
    SQL = ["update ",Table," set ",Set," where ",atom_to_list(KeyField),"=",encode(KeyValue)],
    q(SQL),
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
    case ?ADAPTER:query(Type, Db, Q, ParamList) of
        {ok, Response} ->
            Response;
        {error, no_pool} ->
            error_logger:info_msg("First Request to Database '~p'. Attempting to connect.", [Db]),
            connect(Db),
            db_q(Type, Db, Q, ParamList)
    end.

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

%% fr = First Record
-spec fr(Q :: sql(), ParamList :: [value()]) -> list() | not_found.
fr(Q,ParamList) ->
    case q(Q,ParamList) of
        [] -> not_found;
        [[undefined]] -> not_found;
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
table_fields(Table) ->
    DB = atom_to_list(db()),
    SQL = "select column_name from information_schema.columns where table_schema='" ++ DB ++ " and table_name='" ++ Table ++ "';",
    [list_to_atom(F) || F <- ffl(SQL)].

-spec fields(Table :: table()) -> [atom()].
fields(Table) ->
    table_fields(Table).

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
    fffr(["select ",Field," from ",Table," where ",IDField,"= ?"],[IDValue]).

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
    qu(["delete from ",Table," where ",KeyField,"=?"],[ID]).

-spec encode(V :: any()) -> binary().
%% @doc Safely encodes text for insertion into a query.  Replaces the atoms
%% 'true' and 'false' with <<"1">> and <<"0">> respectively.
encode(V) ->
    ?ADAPTER:encode(V).

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
    base64:encode_to_string(term_to_binary(Data)).

-spec decode64(T :: any()) -> string().
%% @doc Decodes a base64 string into the relevant erlang term.
decode64("") -> "";
decode64(undefined) -> "";
decode64(Data) ->
    binary_to_term(base64:decode(Data)).

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
limit_clause(PerPage, Page) ->
    Offset = offset(PerPage, Page),
    [" limit ",integer_to_list(Offset),", ",integer_to_list(PerPage)].

offset(PerPage, Page) when Page =< 0 ->
    offset(PerPage, 1);
offset(PerPage, Page) when PerPage < 1 ->
    offset(1, Page);
offset(PerPage, Page) when Page > 0 ->
    (Page-1) * PerPage.

