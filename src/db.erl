%% vim: ts=4 sw=4 et
-module(db).
-compile(export_all).

-define(WARNING(QueryText,Msg), error_logger:info_msg("QUERY WARNING: ~p~n~nQuery:~n~p~n~n",[Msg,QueryText])).

-define(ENV(Var, Def), (get_env(Var, Def))).
-define(TYPE,   ?ENV(type, mysql)).
-define(HOST,   ?ENV(host, "127.0.0.1")).
-define(PORT,   ?ENV(port, 3306)).
-define(USER,   ?ENV(user, "root")).
-define(PASS,   ?ENV(pass, "")).
-define(LOOKUP, ?ENV(lookup, fun() -> throw({sigma_sql,undefined_lookup_method}) end )).
-define(CPP,    ?ENV(connections_per_pool, 10)).

%% does not currently do anything
-define(AUTOGROW, application:get_env(sigma_sql, autogrow_pool, false)).

-define(DB, sigma_sql_cached_db).

-type sql()         :: iolist().
-type sql_result()  :: any().
-type db()          :: atom().
-type table()       :: string() | atom().
-type field()       :: string() | atom().
-type value()       :: string() | binary() | atom() | integer() | float().
-type insert_id()   :: term().
-type update_id()   :: term().
-type proplist()    :: {atom() | value()}.
-type return_type() :: dict | list | proplist | tuple.

-spec get_env(Var :: atom(), Def :: term()) -> term().
get_env(Var, Def) ->
    case application:get_env(sigma_sql, Var) of
        undefined -> Def;
        {ok, Val} -> Val
    end.

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
    case ?TYPE of
        mysql -> 
                application:start(crypto),
                application:start(emysql);
        Other -> throw({unknown_db_type, Other})
    end,
    ok.

-spec connect() -> db().
% @doc establishes a connection to the appropriate database.
connect() ->
    connect(db()).

-spec connect(db()) -> db().
% @doc establishes a connection to the named database.
connect(DB) when is_atom(DB) ->
    emysql:add_pool(DB, ?CPP, ?USER, ?PASS, ?HOST, ?PORT, atom_to_list(DB), utf8),
    DB.

-spec pl(Table :: table(), Proplist :: proplist()) -> insert_id() | update_id().
% @doc Shortcut for pl(Table, Table ++ "id", PropList)
pl(Table,PropList) when is_atom(Table) ->
    pl(atom_to_list(Table),PropList);
pl(Table,PropList) when is_list(Table) ->
    KeyField = Table ++ "id",
    pl(Table,KeyField,PropList).

-spec pl(Table :: table(), KeyField :: field(), PropList :: proplist()) -> insert_id() | update_id().
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
        Zero when Zero==0;Zero=="0";Zero==undefined -> 
            pli(Table,pl:delete(PropList,KeyField));
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

-spec dq(Q :: sql()) -> [dict()].
%% @doc Same as q/1, but returns a list of dicts.
dq(Q) ->
    Db = db(),
    db_q(dict,Db,Q).

-spec dq(Q :: sql(), ParamList :: [value()]) -> [dict()].
%% @doc Same as q/2, but returns a list of dicts
dq(Q,ParamList) ->
    Db = db(),
    db_q(dict,Db,Q,ParamList).

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

-spec pli(Table :: table(), PropList :: proplist()) -> insert_id().
%% @doc Inserts a proplist into the table
pli(Table,PropList) when is_atom(Table) ->
    pli(atom_to_list(Table),PropList);
pli(Table,InitPropList) ->
    PropList = filter_fields(Table,InitPropList),
    Sets = [[atom_to_list(F),"=",encode(V)] || {F,V} <- PropList],
    Set = iolist_join(Sets,","),
    SQL = ["insert into ",Table," set ",Set],
    qi(SQL).

-spec plu(Table :: table(), PropList :: proplist()) -> update_id().
%% @doc Updates a row from the proplist based on the key `Table ++ "id"` in the Table
plu(Table,PropList) when is_atom(Table) ->
    plu(atom_to_list(Table),PropList);
plu(Table,PropList) ->
    KeyField = list_to_atom(Table ++ "id"),
    plu(Table,KeyField,PropList).

-spec plu(Table :: table(), KeyField :: field(), PropList :: proplist()) -> update_id().
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
                                                            | update_id()
                                                            | [list() | dict() | tuple() | proplist()].
%% @doc Query from the specified Database pool (Db) This will connect to the
%% specified Database Pool Type must be atoms: proplist, dict, list, or tuple
%% Type can also be atom 'insert' in which case, it'll return the insert value
db_q(Type,Db,Q) ->
    try 
        Res = emysql:execute(Db,Q),
        case emysql_util:result_type(Res) of
            result ->
                format_result(Type,Res);
            ok ->
                case Type of
                    insert -> emysql_util:insert_id(Res);
                    _ ->      emysql_util:affected_rows(Res)
                end;
            error ->
                %% if no connection in pool available ->
                %%      NewDB = connect(),
                %%      db_q(Type,NewDB,Q);
                {error, Res}
        end
    catch
        exit:pool_not_found ->
            connect(Db),
            db_q(Type, Db, Q)
    end.

-spec db_q(Type :: return_type(), Db :: db(),
           Q :: sql(), ParamList :: [value()]) ->   insert_id() 
                                                  | update_id()
                                                  | [list() | dict() | tuple() | proplist()].

db_q(Type,Db,Q,ParamList) ->
    NewQ = q_prep(Q,ParamList),
    db_q(Type,Db,NewQ).

-spec format_result(Type :: return_type(), Res :: sql_result()) ->   list()
                                                                   | dict()
                                                                   | tuple()
                                                                   | proplist().
format_result(Type,Res) ->
    Json = emysql_util:as_json(Res),
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
format_value(null) ->
    undefined;
format_value(V) when is_binary(V) ->
    binary_to_list(V);
format_value(V) ->
    V.

-spec format_key(K :: field()) -> atom().
format_key(K) when is_atom(K) ->
    K;
format_key(K) when is_binary(K) ->
    format_key(binary_to_list(K));
format_key(K) when is_list(K) ->
    list_to_atom(K).

format_list_result(Json) ->
    [
        [format_value(Value) || {_,Value} <- Row]
    || Row <- Json].

format_tuple_result(Json) ->
    [list_to_tuple(Row) || Row <- format_list_result(Json)].

format_proplist_result(Json) ->
    [
        [{format_key(F), format_value(V)} || {F,V} <- Row]
    || Row <-Json].

format_dict_result(Json) ->
    [dict:from_list(PL) || PL <- format_proplist_result(Json)].


%%  A special Query function just for inserting.
%%  Inserts the record(s) and returns the insert_id
qi(Q) ->
    Db = db(),
    db_q(insert,Db,Q).

qi(Q,ParamList) ->
    Db = db(),
    db_q(insert,Db,Q,ParamList).

qu(Q) ->
    Db = db(),
    db_q(update, Db, Q).

qu(Q, ParamList) ->
    Db = db(),
    db_q(update, Db, Q, ParamList).


%% fr = First Record
plfr(Q,ParamList) ->
    case plq(Q,ParamList) of
        [] -> not_found;
        [[undefined]] -> not_found;
        [First|_] -> First
    end.

plfr(Q) ->
    plfr(Q,[]).

tfr(Q) ->
    tfr(Q,[]).

tfr(Q,ParamList) ->
    case tq(Q,ParamList) of
        [] -> not_found;
        [[undefined]] -> not_found;
        [First|_] -> First
    end.

%% fr = First Record
fr(Q,ParamList) ->
    case q(Q,ParamList) of
        [] -> not_found;
        [[undefined]] -> not_found;
        [First|_] -> First
    end.

fr(Q) ->
    fr(Q,[]).

%% fffr = First Field of First record
fffr(Q,ParamList) ->
    case fr(Q,ParamList) of
        not_found -> not_found;
        [First|_] -> First
    end.

fffr(Q) ->
    fffr(Q,[]).

%% First Field List
ffl(Q,ParamList) ->
    [First || [First | _ ] <- db:q(Q,ParamList)].

ffl(Q) ->
    ffl(Q,[]).

%% deprecate this. Use "fields"
table_fields(Table) when is_atom(Table) ->
    table_fields(atom_to_list(Table));
table_fields(Table) ->
    [list_to_atom(F) || F <- db:ffl(["describe ",Table])].

fields(Table) ->
    table_fields(Table).

%% Existance query, just returns true if the query returns anything other than an empty set
%% QE = "Q Exists"
%% TODO: Check for "limit" clause and add? Or rely on user.
qexists(Q) ->
    qexists(Q,[]).

qexists(Q,ParamList) ->
    case q(Q,ParamList) of
        [] -> false;
        [_] -> true;
        [_|_] -> 
            ?WARNING({Q,ParamList},"qexists returned more than one record. Recommend returning one record for performance."),
            true
    end.

exists(Table, IDValue) when is_atom(Table) ->
    exists(atom_to_list(Table), IDValue);
exists(Table, IDValue) when is_list(Table) ->
    exists(Table, list_to_atom(Table) ++ "id", IDValue).

exists(Table, KeyField, IDValue) ->
    case field(Table, KeyField, KeyField, IDValue) of
        not_found -> false;
        _ -> true
    end.

%% retrieves a field value from a table
%% ie: Select 'Field' from 'Table' where 'IDField'='IDValue'
%% This should only be called from the db_ modules.  Never in the page.
%% It's not a security thing, just a convention thing
field(Table,Field,IDField,IDValue) when is_atom(Table) ->
    field(atom_to_list(Table),Field,IDField,IDValue);
field(Table,Field,IDField,IDValue) when is_atom(Field) ->
    field(Table,atom_to_list(Field),IDField,IDValue);
field(Table,Field,IDField,IDValue) when is_atom(IDField) ->
    field(Table,Field,atom_to_list(IDField),IDValue);
field(Table,Field,IDField,IDValue) ->
    db:fffr(["select ",Field," from ",Table," where ",IDField,"= ?"],[IDValue]).

%% This does the same as above, but uses Table ++ "id" for the idfield
field(Table,Field,IDValue) when is_atom(Table) ->
    field(atom_to_list(Table),Field,IDValue);
field(Table,Field,IDValue) ->
    field(Table,Field,Table ++ "id",IDValue).

delete(Table,ID) when is_atom(Table) ->
    delete(atom_to_list(Table),ID);
delete(Table,ID) when is_list(Table) ->
    KeyField = Table ++ "id",
    delete(Table,KeyField,ID).

delete(Table,KeyField,ID) when is_atom(Table) ->
    delete(atom_to_list(Table),KeyField,ID);
delete(Table,KeyField,ID) when is_atom(KeyField) ->
    delete(Table,atom_to_list(KeyField),ID);
delete(Table,KeyField,ID) ->
    db:q(["delete from ",Table," where ",KeyField,"=?"],[ID]).

%%% Prepares a query with Parameters %%%%%%%%%%
q_prep(Q,[]) ->
    Q;
q_prep(Q,ParamList) ->
    QParts = re:split(Q,"\\?",[{return,list}]),
    NumParts = length(QParts)-1,
    NumParams = length(ParamList),
    if
         NumParts == NumParams -> q_join(QParts,ParamList);
         true -> 
             throw({error, "Parameter Count in query is not consistent: ?'s = " ++ integer_to_list(NumParts) ++ ", Params = " ++ integer_to_list(NumParams),[{sql,Q},{params,ParamList}]})
    end.

q_join([QFirstPart|[QSecondPart|QRest]],[FirstParam|OtherParam]) when is_list(QFirstPart);is_list(QSecondPart) ->
    NewFirst = [QFirstPart,encode(FirstParam),QSecondPart],
    q_join([NewFirst|QRest],OtherParam);
q_join([QFirstPart | [QRest]],[FirstParam | [] ]) when is_list(QFirstPart);is_list(QRest) ->
    [QFirstPart,FirstParam,QRest];
q_join([QFirstPart], []) ->
    QFirstPart.

%% Prelim Encoding, then does mysql encoding %%%
%% primarily for the atoms true and false
encode(true) -> <<"1">>;
encode(false) -> <<"0">>;
encode(Other) -> emysql_util:encode(Other).

remove_wrapping_quotes(Str) ->
    lists:reverse(tl(lists:reverse(tl(Str)))).


encode64("") -> "";
encode64(undefined) -> "";
encode64(Data) ->
    base64:encode_to_string(term_to_binary(Data)).

decode64("") -> "";
decode64(undefined) -> "";
decode64(Data) ->
    binary_to_term(base64:decode(Data)).

%% Takes a list of items and encodes them for SQL then returns a
%% comma-separated list of them
encode_list(List) ->
    NewList = [encode(X) || X<-List],
    iolist_join(NewList,",").


dict_to_proplist(SrcDict,AcceptableFields) ->
    DictFilterFoldFun = fun(F,Dict) ->
        case dict:is_key(F,Dict) of
            true -> Dict;
            false -> dict:erase(F,Dict)
        end
    end,
    FilteredDict = lists:foldl(DictFilterFoldFun,SrcDict,AcceptableFields),
    dict:to_list(FilteredDict).

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

offset(PerPage, Page) when Page =< 0 ->
    offset(PerPage, 1);
offset(PerPage, Page) when PerPage < 1 ->
    offset(1, Page);
offset(PerPage, Page) when Page > 0 ->
    (Page-1) * PerPage.

limit_clause(PerPage, Page) ->
    Offset = offset(PerPage, Page),
    [" limit ",integer_to_list(Offset),", ",integer_to_list(PerPage)].
