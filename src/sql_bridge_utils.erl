%% vim: ts=4 sw=4 et
-module(sql_bridge_utils).
-export([
    get_env/2,
    start_poolboy_pool/3,
    to_string/1,
    with_poolboy_pool/2,
    q_prep/2,
    q_join/2
]).

start_poolboy_pool(Name, WorkerArgs, WorkerModule) ->
    Size = get_env(connections_per_pool, 10),
    Overflow = get_env(overflow_connections_per_pool, 10),
    PoolArgs = [
        {name, {local, Name}},
        {size, Size},
        {max_overflow, Overflow},
        {worker_module, WorkerModule}
    ],
    ChildSpec = poolboy:child_spec({local, Name}, PoolArgs, WorkerArgs),
    supervisor:start_child(sql_bridge_sup, ChildSpec).

get_env(Var, Def) ->
    case application:get_env(sql_bridge, Var) of
        undefined -> Def;
        {ok, Val} -> Val
    end.

with_poolboy_pool(DB, Fun) ->
    Worker = poolboy:checkout(DB),
    Return = Fun(Worker),
    poolboy:checkin(DB, Worker),
    Return.


to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(B) when is_binary(B) ->
    binary_to_list(B);
to_string(L) when is_list(L) ->
    L.

%%-spec q_prep(Q :: sql(), ParamList :: [value()]) -> sql().
%% @doc Prepares a query with Parameters, replacing all question marks with the
%% values provided in ParamList.  Returns the newly generated SQL statement as
%% an iolist
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

q_join([QFirstPart|[QSecondPart|QRest]],[FirstParam|OtherParam])
        when is_list(QFirstPart);
             is_list(QSecondPart) ->
    NewFirst = [QFirstPart,sql_bridge:encode(FirstParam),QSecondPart],
    q_join([NewFirst|QRest],OtherParam);
q_join([QFirstPart | [QRest]],[FirstParam | [] ])
        when is_list(QFirstPart);
             is_list(QRest) ->
    [QFirstPart,FirstParam,QRest];
q_join([QFirstPart], []) ->
    QFirstPart.
