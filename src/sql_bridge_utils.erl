%% vim: ts=4 sw=4 et
-module(sql_bridge_utils).
-export([
    get_env/2,
    start_poolboy_pool/3,
    to_string/1,
    with_poolboy_pool/2,
    q_prep/2,
    q_join/2,
    binary_to_string/1,
    stringify_binaries/0,
    replacement_token/0 ,
    placeholder_mysql_to_postgres/2,
    placeholder_postgres_to_mysql/2
]).

replacement_token() ->
    case get_env(replacement_token_style, postgres) of
        postgres -> postgres;
        '$' -> postgres;
        mysql -> mysql;
        '?' -> mysql
    end.

stringify_binaries() ->
    get_env(stringify_binaries, false).

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

%% This is only used by the dynamically build module 'sql_bridge_stringify'.
%% Made in the sql_bridge_alias module
binary_to_string(B) when is_binary(B) ->
   binary_to_list(B);
binary_to_string(L) when is_list(L) ->
    L;
binary_to_string(Other) ->
    Other.

%%-spec q_prep(Q :: sql(), ParamList :: [value()]) -> sql().
%% @doc Prepares a query with Parameters, replacing all question marks with the
%% values provided in ParamList.  Returns the newly generated SQL statement as
%% an iolist
q_prep(Q,[]) ->
    Q;
q_prep(Q,ParamList) ->
    QParts = re:split(Q,"\\?",[{return,list}]),
    ok = verify_same_param_count(Q, QParts, ParamList).

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


%% Convert "?, ?, ?" to "%1, %2, %3"
placeholder_mysql_to_postgres(Q, Params) ->
    QParts = re:split(Q, "\\?", [{return, binary}]),
    ok = verify_same_param_count(Q, QParts, Params),
    {iolist_to_binary(postgres_join(QParts, 1)), Params}.


postgres_join([QFirstPart], _) ->
    QFirstPart;
postgres_join([QFirstPart|QRest], ParamNum) ->
    ParamToken = ["$", integer_to_list(ParamNum)],
    [QFirstPart, ParamToken | postgres_join(QRest, ParamNum+1)].


placeholder_postgres_to_mysql(Q0, OrigParams) ->
    Q = binary_to_list(iolist_to_binary(Q0)),
    {_NewQ, _NewParams} = p_to_m(Q, OrigParams, [], []).

p_to_m([$$, X, Y | Rest], OrigParams, QAcc, ReorderedParams)
        when X >= $1, X =< $9, Y >= $0, Y =< $9 ->
    N = (X-$0)*10 + (Y-$0),
    Param = lists:nth(N, OrigParams),
    NewReorderedParams = [Param | ReorderedParams],
    NewQAcc = [$? | QAcc],
    p_to_m(Rest, OrigParams, NewQAcc, NewReorderedParams);
p_to_m([$$, X | Rest], OrigParams, QAcc, ReorderedParams)
        when X >= $1, X =< $9 ->
    N = X-$0,
    Param = lists:nth(N, OrigParams),
    NewReorderedParams = [Param | ReorderedParams],
    NewQAcc = [$? | QAcc],
    p_to_m(Rest, OrigParams, NewQAcc, NewReorderedParams);
p_to_m([X | Rest], OrigParams, QAcc, ReorderedParams) ->
    NewQAcc = [X | QAcc],
    p_to_m(Rest, OrigParams, NewQAcc, ReorderedParams);
p_to_m([], _OrigParams, QAcc, ReorderedParams) ->
    %% Here's the big hack. The last element of our new list is going to be a tuple containing the UnreversedParams
    UnreversedParams = lists:reverse(ReorderedParams),
    UnreversedQAcc = lists:reverse(QAcc),
    {UnreversedQAcc, UnreversedParams}.


   
verify_same_param_count(Q, QParts, ParamList) ->
    NumParts = length(QParts)-1,
    NumParams = length(ParamList),
    if
         NumParts == NumParams ->
            ok;
         true ->
            {error,
             {query_param_count_inconsistent, [
                {tokens, NumParts},
                {params, NumParams},
                {sql,Q},
                {params,ParamList}
             ]}
            }
    end.

