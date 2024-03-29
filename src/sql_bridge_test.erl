%% vim: ts=4 sw=4 et sts=4
-module(sql_bridge_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% This is just a shortcut to prevent dialyzer throwing errors for the
%% dynamically generated `db` module.
-define(DB, sql_bridge).

-define(P(X), (sql_bridge_utils:create_placeholder(X))).
-define(P1, ?P(1)).
-define(P2, ?P(2)).
-define(P3, ?P(3)).
-define(P4, ?P(4)).
-define(P5, ?P(5)).
-define(P6, ?P(6)).
-define(P7, ?P(7)).
-define(P8, ?P(8)).
-define(P9, ?P(9)).

-define(MYSQL_HOST, os:getenv("MYSQLHOST", "localhost")).
-define(PG_HOST, os:getenv("PGHOST", "localhost")).

mysql_otp_test_() ->
    {setup,
     %% We are intentionally testing postgres-style token replacements on mysql
     %% (so 'postgres' is not a typo here)
     fun() -> gen_setup(sql_bridge_mysql_otp, postgres, ?MYSQL_HOST, 3306) end,
     fun mysql_otp_cleanup/1,
     fun main_tests/1
    }.

mysql_otp_trans_test_() ->
    {setup,
     fun() -> gen_setup(sql_bridge_mysql_otp, mysql, ?MYSQL_HOST, 3306) end,
     fun mysql_otp_cleanup/1,
     fun trans_tests/1
    }.

epgsql_trans_test_() ->
    {setup,
     fun() -> gen_setup(sql_bridge_epgsql, postgres, ?PG_HOST, 5432) end,
     fun epgsql_cleanup/1,
     fun main_tests/1
    }.

epgsql_test_() ->
    {setup,
     %% We are intentionally testing mysql-style token replacements on pgsql
     %% (so 'mysql' is not a typo here)
     fun() -> gen_setup(sql_bridge_epgsql, mysql, ?PG_HOST, 5432) end,
     fun epgsql_cleanup/1,
     fun trans_tests/1
    }.


gen_setup(Adapter, ReplacementType, Host, Port) ->
    error_logger:info_msg("Starting Adapter: ~p~n",[Adapter]),
    application:load(sql_bridge),
    application:set_env(sql_bridge, adapter, Adapter),
    application:set_env(sql_bridge, port, Port),
    application:set_env(sql_bridge, user, "sql_bridge_user"),
    application:set_env(sql_bridge, pass, "sql_bridge_test_password"),
    application:set_env(sql_bridge, host, Host),
    application:set_env(sql_bridge, lookup, sql_bridge_test),
    application:set_env(sql_bridge, replacement_token_style, ReplacementType),
    application:set_env(sql_bridge, stringify_binaries, true),
    sql_bridge:start(),
    ?DB:q("delete from fruit").


epgsql_cleanup(_) ->
    application:stop(epgsql),
    application:stop(sql_bridge),
    ok.

mysql_otp_cleanup(_) ->
    application:stop(mysql),
    application:stop(sql_bridge),
    ok.

trans_tests(_) ->
    LookupPid = erlang:spawn(fun lookup_loop/0),
    [
        {inorder, [
            % Extending the timeout from 15000 to 20000, in case github actions was just running slowly?
            {timeout, 20000, [
                {inparallel, [
                    ?_assert(test_trans(LookupPid, 1)),
                    ?_assert(test_trans(LookupPid, 2)),
                    ?_assert(test_trans(LookupPid, 3)),
                    ?_assert(test_trans(LookupPid, 4)),
                    ?_assert(test_trans(LookupPid, 5)),
                    ?_assert(test_trans(LookupPid, 6)),
                    ?_assert(test_trans(LookupPid, 7)),
                    ?_assert(test_trans(LookupPid, 8)),
                    ?_assert(test_trans(LookupPid, 9)),
                    ?_assert(test_trans(LookupPid, 10)),
                    ?_assert(test_trans(LookupPid, 11)),
                    ?_assert(test_trans(LookupPid, 12)),
                    ?_assertNot(test_trans(LookupPid, 50, rollback)),
                    ?_assertNot(test_trans(LookupPid, 60, rollback)),
                    ?_assertNot(test_trans(LookupPid, 70, rollback))
                ]}
            ]},
            ?_assertEqual(12, ?DB:fffr("select count(*) from fruit")),
            ?_assertEqual(12, ?DB:fffr(["select count(*) from fruit where quantity in (",?DB:encode_list([1,2,3,4,5,6,7,8,9,10,11,12]),")"]))
        ]}
    ].

lookup_loop() ->
    lookup_loop([]).

lookup_loop(Fruitids) ->
    receive
        {register, Fruitid} ->
            lookup_loop(Fruitids ++ [Fruitid]);
        {lookup, Pid} ->
            %% Get the first fruit from thee list
            [Fruitid|Rest] = Fruitids,
            %% send it back to the user
            Pid ! Fruitid,
            %% Put the found fruitid to the end of the list (so we don't grab it again right away)
            NewFruitids = Rest ++ [Fruitid],
            %% then loop again
            lookup_loop(NewFruitids)
    after
        %% if no messages received for 10 seconds, we can safely die1
        10000 -> die
    end.

register_fruitid(LookupPid, Fruitid) ->
    LookupPid ! {register, Fruitid}.

lookup_fruitid(LookupPid, NotFruitid) ->
    LookupPid ! {lookup, self()},
    receive
        NotFruitid ->
            %% We happened to retrieve ourselves. We don't want that. Try again (the queue will change with each request)
            lookup_fruitid(LookupPid, NotFruitid);
        Fruitid ->
            Fruitid
    after
        10000 -> throw(not_received)
    end.

test_trans(LookupPid, Quantity) ->
    test_trans(LookupPid, Quantity, commit).

-define(TRANS_STATUS(Msg, Args), trans_status(StartTime, FruitName, Msg, Args)).
-define(TRANS_STATUS(Msg), ?TRANS_STATUS(Msg, [])).

test_trans(LookupPid, Quantity, CommitOrRollback) ->
    FruitName = "Fruit-" ++ integer_to_list(Quantity),
    SleepModifier = Quantity * 10,
    _AddedFruitid = ?DB:trans(fun() ->
        FirstSleep = 1000 - SleepModifier,
        SecondSleep = 1500 + SleepModifier,
        ThirdSleep = 2000,
        StartTime = os:timestamp(),
        ?TRANS_STATUS("Transaction Started. Sleeping for ~pms", [FirstSleep]),
        timer:sleep(FirstSleep),
        ?TRANS_STATUS("Woke up. Verifying fruit table is empty"),
        0=?DB:fffr("select count(*) from fruit"),
        ?TRANS_STATUS("Inserting ~s", [FruitName]),
        Fruitid = ?DB:qi(["insert into fruit(fruit, quantity) values(",?P1,",",?P2,")"], [FruitName, Quantity]),
        ?TRANS_STATUS("Inserted (fruitid=~p). Registering with tracker process",[Fruitid]),
        register_fruitid(LookupPid, Fruitid),
        ?TRANS_STATUS("Registered. Verifying that Friutid=~p exists in transaction.", [Fruitid]),
        true=?DB:exists(fruit, Fruitid),
        ?TRANS_STATUS("fruitid=~p exists. Counting records in table (should only be 1)", [Fruitid]),
        1=?DB:fffr("select count(*) from fruit"),
        ?TRANS_STATUS("Verified. Sleeping for ~pms", [SecondSleep]),
        timer:sleep(SecondSleep),
        ?TRANS_STATUS("Woke up. Getting a random other fruit that was inserted in another transaction."),
        OtherTranFruitid = lookup_fruitid(LookupPid, Fruitid),
        ?TRANS_STATUS("Retrieved other fruitid=~p. Verifying its validity.",[OtherTranFruitid]),
        true=is_integer(OtherTranFruitid),
        true=(Fruitid=/=OtherTranFruitid),
        ?TRANS_STATUS("Verified. Verifying that fruitid=~p does not yet exists in this transaction.",[OtherTranFruitid]),
        false=?DB:exists(fruit, OtherTranFruitid),
        ?TRANS_STATUS("Verified. Sleeping for ~pms.",[ThirdSleep]),
        timer:sleep(ThirdSleep),
        ?TRANS_STATUS("Woke up. Now checking if we should crash or return."),
        %% this will crash if CommitOrRollback=rollback, causing the
        %% transaction to be rolled back completely (or it should be, anyway)
        commit=CommitOrRollback,
        Fruitid
    end),
    ?DB:qexists(["select * from fruit where quantity=",?P1], [Quantity]).

%sleep_random(Min, Max) ->
%    Time = crypto:rand_uniform(Min, Max),
%    timer:sleep(Time).

trans_status(StartTime, Tag, Msg, Args) ->
    Now = os:timestamp(),
    Microsec = timer:now_diff(Now, StartTime),
    ElapsedMS = Microsec div 1000,
    Args2 = [self(), Tag, ElapsedMS] ++ Args,
    Msg2 = "(~p) Trans Update [Tag = ~p] (~pms Elapsed): " ++ Msg ++ "\n",
    logger:notice(Msg2, Args2).

main_tests(_) ->
    [
     ?_assertEqual([], ?DB:q("select * from fruit")),
     ?_assertEqual([], ?DB:tq("select * from fruit")),
     ?_assertEqual([], ?DB:dq("select * from fruit")),
     ?_assertEqual([], ?DB:mq("select * from fruit")),
     ?_assertEqual([], ?DB:plq("select * from fruit")),
     ?_assertEqual(not_found, ?DB:fr("select * from fruit")),
     ?_assertEqual(not_found, ?DB:tfr("select * from fruit")),
     ?_assertEqual(not_found, ?DB:mfr("select * from fruit")),
     ?_assertEqual(not_found, ?DB:plfr("select * from fruit")),
     ?_assertEqual(not_found, ?DB:dfr("select * from fruit")),

     ?_assertMatch([fruitid, fruit, description, quantity, picture, some_float], ?DB:table_fields(fruit)),
     ?_assert(is_integer(?DB:qi(["insert into fruit(fruit, quantity, some_float) values(", ?P1, ",", ?P2, ",", ?P3,")"], ["apple", 5, 10.1]))),
     ?_assertEqual(undefined, ?DB:fffr("select description from fruit where fruit='apple'")),
     ?_assertEqual(5, ?DB:fffr(["select quantity from fruit where fruit=",?P1 ], [apple])),
     ?_assertEqual("apple", ?DB:fffr(["select fruit from fruit where quantity=", ?P1], [5])),
     ?_assertEqual("apple", ?DB:fffr(["select fruit from fruit where quantity=", ?P1], ["5"])),
     ?_assertEqual("apple", ?DB:fffr(["select fruit from fruit where quantity=", ?P1], [<<"5">>])),
     ?_assert(is_integer(?DB:pl(fruit, [{fruitid, 0}, {fruit, <<"banana">>}, {quantity, 100}, {description, "long and yellow"}, {some_float, 6.1}]))),
     ?_assert(is_float(?DB:fffr("select sum(some_float) from fruit"))),
     ?_assertMatch("long and yellow", ?DB:field(fruit, description, fruit, "banana")),

     ?_assertEqual([["apple", 5], ["banana", 100]], ?DB:q("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual(["apple", 5], ?DB:fr("select fruit, quantity from fruit order by fruit")),

     ?_assertEqual([{"apple", 5}, {"banana", 100}], ?DB:tq("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual({"apple", 5}, ?DB:tfr("select fruit, quantity from fruit order by fruit")),

     ?_assertEqual([[{fruit, "apple"}, {quantity, 5}], [{fruit, "banana"}, {quantity, 100}]], ?DB:plq("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual([{fruit, "apple"}, {quantity, 5}], ?DB:plfr("select fruit, quantity from fruit order by fruit")),

     ?_assertEqual("apple", dict:fetch(fruit, ?DB:dfr("select fruit, quantity from fruit order by fruit"))),

     ?_assertEqual([#{fruit=>"apple", quantity=>5}, #{fruit=>"banana", quantity=>100}], ?DB:mq("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual(#{fruit=>"apple", quantity=>5},  ?DB:mfr("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual(["apple", "banana"], ?DB:ffl("select fruit from fruit order by fruit")),
     ?_assert(?DB:qexists(["select * from fruit where fruit=",?P1], [banana])),
     ?_assert(?DB:qexists(["select * from fruit where fruit=",?P1], ["apple"])),
     ?_assertNot(?DB:qexists(["select * from fruit where fruit=",?P1], [<<"watermelon">>])),
     ?_assertEqual(#{fruit=>"orange", quantity=>5, description=>"oranges are orange"}, update_apple_to_orange()),
     ?_assertEqual("berry", test_insert_id()),
     ?_assertEqual(1, ?DB:delete(fruit, fruit, "orange")),
     ?_assert(test_exists("banana")),
     ?_assertNot(?DB:exists(fruit, fruit, "banana-fake")),
     ?_assertNot(test_id_delete()),
     ?_assert(test_string("ﻦﺤﻧ ﺫﺎﻬﺑﻮﻧ ﻒﻳ ﺡﺎﺟﺓ ﺈﻟﻯ ﻕﺍﺮﺑ ﺄﻜﺑﺭ")),
     ?_assert(test_string("我们将需要更大的船")),
     ?_assert(test_string("Budeme potřebovat větší loď")),
     ?_assert(test_string("ჩვენ ვაპირებთ, რომ უნდა დიდი ნავი")),
     ?_assert(test_string("Мы собираемся нуждаться в большей лодку")),
     ?_assert(test_string("testy'pants")),
     ?_assert(test_string("'+\"!@#$%^&*()\\//\\//';[]<>./-=-=+")),
     ?_assert(test_encode_list(["'+\"", "!@#$'^&%", "//\\//\\", "blah","123","-=--=-!+'\"'''''''''''''''''"])),
     ?_assertEqual({some, crazy,"term"}, ?DB:decode64(?DB:encode64({some, crazy,"term"}))),
     ?_assertMatch([_, _], ?DB:q("select * from fruit " ++ ?DB:limit_clause(2, 1))),
     ?_assertMatch([_, _], ?DB:q("select * from fruit " ++ ?DB:limit_clause(2, -1))),
     ?_assertMatch([_], ?DB:q("select * from fruit " ++ ?DB:limit_clause(-123, 5))),
     ?_assertEqual(1.1, test_float(1.1)),
     ?_assertEqual(12345.5, test_float(12345.5)),
     ?_assertEqual(12.5, test_decimal(12.5)),
     ?_assertEqual(undefined, test_null()),
     ?_assertEqual("2016-12-31", test_date("2016-12-31")),
     ?_assertEqual("23:00:00", test_time("23:00:00")),
     ?_assertEqual("2016-12-31 23:00:00", test_datetime("2016-12-31 23:00:00"))
    ].

test_decimal(V) ->
    test_in_out_other(my_decimal, V).

test_date(V) ->
    test_in_out_other(my_date, V).

test_time(V) ->
    test_in_out_other(my_time, V).

test_datetime(V) ->
    test_in_out_other(my_datetime, V).

test_null() ->
    ID = ?DB:pl(other, [{otherid, 0}, {my_decimal, 123.5}]),
    ?DB:field(other, my_date, ID).

test_in_out_other(Field, V) ->
    ID = ?DB:pl(other, [{Field, V}]),
    ?DB:field(other, Field, ID).

test_float(Val) ->
    Fruitid = ?DB:pl(fruit, [{some_float, Val}]),
    ?DB:field(fruit, some_float, Fruitid).

update_apple_to_orange() ->
    Fruitid = ?DB:fffr(["Select fruitid from fruit where fruit=",?P1], ["apple"]),
    New = [
        {fruitid, Fruitid},
        {fruit, "orange"},
        {description, "oranges are orange"}
    ],
    ?DB:pl(fruit, New),
    ?DB:mfr(["Select fruit, quantity, description from fruit where fruitid=", ?P1], [Fruitid]).

test_insert_id() ->
    Fruitid = ?DB:qi("insert into fruit(fruit, quantity) values('berry', 200)"),
    ?DB:field(fruit, fruit, Fruitid).

test_exists(Fruit) ->
    Fruitid = ?DB:fffr(["select fruitid from fruit where fruit=",?P1], [Fruit]),
    ?DB:exists(fruit, Fruitid).

test_id_delete() ->
    Fruitid = ?DB:fffr(["select fruitid from fruit where fruit=",?P1], ["banana"]),
    ?DB:delete(fruit, Fruitid),
    ?DB:exists(fruit, Fruitid).

test_string(Str) ->
    Fruitid = ?DB:pl(fruit, [{fruitid, 0}, {fruit, "new"}, {description, Str}]),
    Str == ?DB:field(fruit, description, Fruitid).

test_encode_list(List) ->
    Fruitids = lists:map(fun(Fruit) ->
        ?DB:pl(fruit, [{fruit, Fruit}])
    end, List),
    Fruitids = ?DB:ffl(["select fruitid from fruit where fruitid in (",?DB:encode_list(Fruitids),") order by fruitid"]),
    Fruitids = ?DB:ffl(["select fruitid from fruit where fruit in (", ?DB:encode_list(List), ") order by fruitid"]),
    true.


