%% vim: ts=4 sw=4 et
-module(sql_bridge_utils).
-export([
    get_env/2,
    record_handler/0,
    convert_record/1,
    start_poolboy_pool/3,
    to_string/1,
    with_poolboy_pool/2,
    q_prep/2,
    q_join/2,
    binary_to_string/1,
    stringify_binaries/0,
    replacement_token/0 ,
    token_mysql_to_postgres/2,
    token_postgres_to_mysql/2,
    create_placeholders/1,
    create_placeholder/1,
    to_atom/1,
    checkout_pool/1,
    checkin_pool/1,
    trans_depth/1,
    trans_deeper/1,
    trans_shallower/1,
    clear_trans_depth/1,
    record_to_proplist/2,
    proplist_to_record/3,
    format_datetime/1,
    log/1,
    log/2
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

record_handler() ->
    case get_env(record_handler, undefined) of
        undefined -> throw("No record handler defined");
        F when is_function(F) -> F;
        {Mod, Fun} -> fun Mod:Fun/1
    end.

convert_record(Record) ->
    Handler = record_handler(),
    Handler(Record).

start_poolboy_pool(Name, WorkerArgs, WorkerModule) ->
    Size = get_env(connections_per_pool, 10),
    Overflow = get_env(overflow_connections_per_pool, 10),
    PoolArgs = [
        {name, {local, Name}},
        {size, Size},
        {max_overflow, Overflow},
        {worker_module, WorkerModule}
    ],
    %ChildSpec = poolboy:child_spec({local, Name}, PoolArgs, WorkerArgs, map),
    %Poolid = {local, Name},
    
    ChildSpec = #{
        id => {local, Name},
        start => {poolboy, start_link, [PoolArgs, WorkerArgs]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [poolboy]
    },


    supervisor:start_child(sql_bridge_sup, ChildSpec).

get_env(Var, Def) ->
    case application:get_env(sql_bridge, Var) of
        undefined -> Def;
        {ok, Val} -> Val
    end.

with_poolboy_pool(DB, Fun) ->
    case erlang:get({sql_bridge_current_pool, DB}) of
        undefined ->
            Worker = poolboy:checkout(DB),
            Return = Fun(Worker),
            poolboy:checkin(DB, Worker),
            Return;
        {ok, Worker} ->
            _Return = Fun(Worker)
    end.

%% When checking out, we make sure to check if we already have one checked out,
%% and if so, we "go deeper" instead of checking another connection out. The %%
%% first time checking out a connection_pool brings us to a depth of 1. A depth of
%% 0 means "not checked out" %% when checking in, we also make sure to check how
%% deep we are, and if we reach a depth of 1, we're at the last depth, so we
%% can clear the depth.

checkout_pool(DB) ->
    %% Are we checked out already? 0 = no, X >= 1 means yes.
    case checkout_depth(DB) of
        0 ->
            %% Not checked out, so let's check out, and "go deeper" (bringing us to an initial depth of 1)
            Worker = poolboy:checkout(DB),
            erlang:put({sql_bridge_current_pool, DB}, {ok, Worker}),
            checkout_deeper(DB),
            ok;
        X when X >= 1 ->
            %% already checked out, just go deeper
            checkout_deeper(DB),
            ok
    end.

checkin_pool(DB) ->
    %% Are we checked out already?
    case checkout_depth(DB) of
        1 -> 
            %% We're at a depth of 1 wihhc menas we're checked out, but this is
            %% the last checkout, so we can safely check back in
            {ok, Worker} = erlang:get({sql_bridge_current_pool, DB}),
            poolboy:checkin(DB, Worker),
            erlang:put({sql_bridge_current_pool, DB}, undefined),
            clear_checkout_depth(DB),
            ok;
        X when X > 1 ->
            %% Yep, we're checked out, and it's not the last depth, so let's just how shallower
            checkout_shallower(DB),
            ok;
        X when X =< 0 ->
            %% something went wrong. We should never be attempting to check in
            %% at or less than 0. Throw an error
            erlang:exit({invalid_depth_to_check_in, [{db, DB}, {depth, X}]})
    end.


checkout_depth(DB) ->
    case erlang:get({checkout_depth, DB}) of
        undefined -> 0;
        X when is_integer(X) -> X
    end.

checkout_depth(DB, Val) ->
    erlang:put({checkout_depth, DB}, Val).

clear_checkout_depth(DB) ->
    erlang:erase({checkout_depth, DB}).

checkout_deeper(DB) ->
    Depth = checkout_depth(DB),
    NewDepth = Depth + 1,
    checkout_depth(DB, NewDepth),
    NewDepth.

checkout_shallower(DB) ->
    case checkout_depth(DB) of
        0 ->
            0;
        Depth ->
            NewDepth = Depth - 1,
            checkout_depth(DB, NewDepth),
            NewDepth
    end.

trans_depth(DB) ->
    case erlang:get({trans_depth, DB}) of
        undefined -> 0;
        X when is_integer(X) -> X
    end.

trans_depth(DB, Val) ->
    erlang:put({trans_depth, DB}, Val).

clear_trans_depth(DB) ->
    erlang:erase({trans_depth, DB}).

trans_deeper(DB) ->
    Depth = trans_depth(DB),
    NewDepth = Depth + 1,
    trans_depth(DB, NewDepth),
    NewDepth.

trans_shallower(DB) ->
    case trans_depth(DB) of
        0 ->
            0;
        Depth ->
            NewDepth = Depth - 1,
            trans_depth(DB, NewDepth),
            NewDepth
    end.
    

to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(B) when is_binary(B) ->
    binary_to_list(B);
to_string(L) when is_list(L) ->
    L.

%% This is only used by the dynamically build module 'sql_bridge_stringify'.
%% Made in the sql_bridge_alias module
binary_to_string(B) when is_binary(B) ->
   unicode:characters_to_list(B);
binary_to_string(L) when is_list(L) ->
    L;
binary_to_string(Other) ->
    Other.

%%-spec q_prep(Q :: sql(), ParamList :: [value()]) -> sql().
%   % @doc Prepares a query with Parameters, replacing all question marks with the
%% values provided in ParamList.  Returns the newly generated SQL statement as
%% an iolist
q_prep(Q,[]) ->
    Q;
q_prep(Q,ParamList) ->
    QParts = re:split(Q,"\\?",[{return,list}]),
    ok = verify_same_param_count(Q, QParts, ParamList),
    q_join(QParts, ParamList).

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
token_mysql_to_postgres(Q, Params) ->
    QParts = re:split(Q, "\\?", [{return, binary}]),
    ok = verify_same_param_count(Q, QParts, Params),
    {iolist_to_binary(postgres_join(QParts, 1)), Params}.


postgres_join([QFirstPart], _) ->
    QFirstPart;
postgres_join([QFirstPart|QRest], ParamNum) ->
    ParamToken = ["$", integer_to_list(ParamNum)],
    [QFirstPart, ParamToken | postgres_join(QRest, ParamNum+1)].


token_postgres_to_mysql(Q0, OrigParams) ->
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

%% returns a list of placeholders in order
create_placeholders(NumFields) ->
    Fun = case replacement_token() of
        mysql -> fun create_mysql_placeholder/1;
        postgres -> fun create_postgres_placeholder/1
    end,
    [Fun(X) || X <- lists:seq(1, NumFields)].

create_placeholder(Num) ->
    case replacement_token() of
        mysql -> create_mysql_placeholder(Num);
        postgres -> create_postgres_placeholder(Num)
    end.

create_mysql_placeholder(_) ->
    "?".

create_postgres_placeholder(Num) ->
    "$" ++ integer_to_list(Num).

to_atom(B) when is_binary(B) ->
    list_to_atom(binary_to_list(B));
to_atom(A) when is_list(A) ->
    list_to_atom(A);
to_atom(A) when is_atom(A) ->
    A.

record_to_proplist(Record, FieldMap) ->
    NumberedFields = lists:zip(lists:seq(2, length(FieldMap)+1), FieldMap),
    lists:map(fun({ElNum, Field}) ->
        Value = element(ElNum, Record),
        {Field, Value}
    end, NumberedFields).

proplist_to_record(Proplist, Tag, FieldMap) ->
    Vals = lists:map(fun(Field) ->
        proplists:get_value(Field, Proplist, undefined)
    end, FieldMap),
    list_to_tuple([Tag | Vals]).

format_datetime({{Y,M,D},{H,I,S}}) when is_float(S) ->
    format_datetime({{Y,M,D},{H,I,erlang:trunc(S)}});
format_datetime({{Y,M,D},{H,I,S}}) ->
    to_bin_or_str(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y,M,D,H,I,S]));
format_datetime({time, {H,I,S}}) ->
    format_datetime({0, {H,I,S}});
format_datetime({0, {H,I,S}}) when is_integer(S) ->
    to_bin_or_str(io_lib:format("~2..0B:~2..0B:~2..0B", [H,I,S]));
format_datetime({0, {H,I,S}}) when is_float(S) ->
    format_datetime({0, {H,I,erlang:trunc(S)}});
format_datetime({H,I,S}) when is_float(S) ->
    format_datetime({0, {H,I,S}});
format_datetime({Y,M,D}) ->
	to_bin_or_str(io_lib:format("~4..0B-~2..0B-~2..0B", [Y,M,D])).

to_bin_or_str(B) when is_binary(B) ->
    case stringify_binaries() of
        true -> sql_bridge_stringify:maybe_string(B);
        false -> B
    end;
to_bin_or_str(L) when is_list(L) ->
    case stringify_binaries() of
        true -> lists:flatten(L);
        false -> iolist_to_binary(L)
    end.

log(Msg) ->
    log("~p", [Msg]).

log(Msg, Args) ->
    Msg2 = io_lib:format(Msg, Args),
    Pid = self(),
    PreSuf = "***************************************************\n",
    Msg3 = io_lib:format("~p: ~ts\n\n", [Pid, Msg2]),
    io:format("~s~ts~s", [PreSuf, Msg3, PreSuf]).


