%% vim: ts=4 sw=4 et sts=4
-module(sql_bridge_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

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

%mysql_otp_test_() ->
%    {setup,
%     fun() -> gen_setup(sql_bridge_mysql_otp, postgres, 3306) end,
%     fun mysql_otp_cleanup/1,
%     fun main_tests/1
%    }.

mysql_otp_trans_test_() ->
    {setup,
     fun() -> gen_setup(sql_bridge_mysql_otp, mysql, 3306) end,
     fun mysql_otp_cleanup/1,
     fun trans_tests/1
    }.

%epgsql_trans_test_() ->
%    {setup,
%     fun() -> gen_setup(sql_bridge_epgsql, mysql, 5432) end,
%     fun epgsql_cleanup/1,
%     fun main_tests/1
%    }.
%
%epgsql_test_() ->
%    {setup,
%     fun() -> gen_setup(sql_bridge_epgsql, postgres, 5432) end,
%     fun epgsql_cleanup/1,
%     fun trans_tests/1
%    }.


gen_setup(Adapter, ReplacementType, Port) ->
    error_logger:info_msg("Starting Adapter: ~p~n",[Adapter]),
    application:load(sql_bridge),
    application:set_env(sql_bridge, adapter, Adapter),
    application:set_env(sql_bridge, port, Port),
    application:set_env(sql_bridge, user, "sql_bridge_user"),
    application:set_env(sql_bridge, pass, "sql_bridge_test_password"),
    application:set_env(sql_bridge, lookup, sql_bridge_test),
    application:set_env(sql_bridge, replacement_token_style, ReplacementType),
    application:set_env(sql_bridge, stringify_binaries, true),
    sql_bridge:start(),
    ok = db:q("delete from fruit").


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
        {timeout, 15000, [
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
                ?_assertNot(test_trans(LookupPid, 500, rollback)),
                ?_assertNot(test_trans(LookupPid, 600, rollback)),
                ?_assertNot(test_trans(LookupPid, 700, rollback))
            ]}
        ]},
        [
         ?_assertEqual(12, db:fffr("select count(*) from fruit")),
         ?_assertEqual(12, db:fffr(["select count(*) from fruit where quantity in (",db:encode_list([1,2,3,4,5,6,7,8,9,10,11,12]),")"]))
        ]
    ].

lookup_loop() ->
    lookup_loop([]).

lookup_loop(Fruitids) ->
    receive
        {register, Fruitid} ->
            lookup_loop(Fruitids ++ [Fruitid]);
        {lookup, Pid} ->
            [Fruitid|Rest] = Fruitids,
            Pid ! Fruitid,
            lookup_loop(Rest)
    after
        10000 -> die
    end.

register_fruitid(LookupPid, Fruitid) ->
    LookupPid ! {register, Fruitid}.

lookup_fruitid(LookupPid, NotFruitid) ->
    LookupPid ! {lookup, self()},
    receive
        NotFruitid ->
            register_fruitid(LookupPid, NotFruitid),
            lookup_fruitid(LookupPid, NotFruitid);
        Fruitid ->
            Fruitid
    after
        10000 -> throw(not_received)
    end.

test_trans(LookupPid, Quantity) ->
    test_trans(LookupPid, Quantity, commit).

test_trans(LookupPid, Quantity, CommitOrRollback) ->
    FruitName = "Fruit-" ++ integer_to_list(Quantity),
    _AddedFruitid = db:trans(fun() ->
        timer:sleep(1000),
        0=db:fffr("select count(*) from fruit"),
        Fruitid = db:qi(["insert into fruit(fruit, quantity) values(",?P1,",",?P2,")"], [FruitName, Quantity]),
        register_fruitid(LookupPid, Fruitid),
        true=db:exists(fruit, Fruitid),
        1=db:fffr("select count(*) from fruit"),
        timer:sleep(4000),
        OtherTranFruitid = lookup_fruitid(LookupPid, Fruitid),
        true=is_integer(OtherTranFruitid),
        true=(Fruitid=/=OtherTranFruitid),
        false=db:exists(fruit, OtherTranFruitid),
        timer:sleep(4000),
        commit=CommitOrRollback,
        Fruitid
    end),
    db:qexists(["select * from fruit where quantity=",?P1], [Quantity]).

main_tests(_) ->
    [
     ?_assertEqual([], db:q("select * from fruit")),
     ?_assertEqual([], db:tq("select * from fruit")),
     ?_assertEqual([], db:dq("select * from fruit")),
     ?_assertEqual([], db:mq("select * from fruit")),
     ?_assertEqual([], db:plq("select * from fruit")),
     ?_assertEqual(not_found, db:fr("select * from fruit")),
     ?_assertEqual(not_found, db:tfr("select * from fruit")),
     ?_assertEqual(not_found, db:mfr("select * from fruit")),
     ?_assertEqual(not_found, db:plfr("select * from fruit")),
     ?_assertEqual(not_found, db:dfr("select * from fruit")),

     ?_assertMatch([fruitid, fruit, description, quantity, picture, some_float], db:table_fields(fruit)),
     ?_assert(is_integer(db:qi(["insert into fruit(fruit, quantity, some_float) values(", ?P1, ",", ?P2, ",", ?P3,")"], ["apple", 5, 10.1]))),
     ?_assertEqual(undefined, db:fffr("select description from fruit where fruit='apple'")),
     ?_assertEqual(5, db:fffr(["select quantity from fruit where fruit=",?P1 ], [apple])),
     ?_assertEqual("apple", db:fffr(["select fruit from fruit where quantity=", ?P1], [5])),
     ?_assert(is_integer(db:pl(fruit, [{fruitid, 0}, {fruit, <<"banana">>}, {quantity, 100}, {description, "long and yellow"}, {some_float, 6.1}]))),
     ?_assert(is_float(db:fffr("select sum(some_float) from fruit"))),
     ?_assertMatch("long and yellow", db:field(fruit, description, fruit, "banana")),

     ?_assertEqual([["apple", 5], ["banana", 100]], db:q("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual(["apple", 5], db:fr("select fruit, quantity from fruit order by fruit")),

     ?_assertEqual([{"apple", 5}, {"banana", 100}], db:tq("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual({"apple", 5}, db:tfr("select fruit, quantity from fruit order by fruit")),

     ?_assertEqual([[{fruit, "apple"}, {quantity, 5}], [{fruit, "banana"}, {quantity, 100}]], db:plq("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual([{fruit, "apple"}, {quantity, 5}], db:plfr("select fruit, quantity from fruit order by fruit")),

     ?_assertEqual("apple", dict:fetch(fruit, db:dfr("select fruit, quantity from fruit order by fruit"))),

     ?_assertEqual([#{fruit=>"apple", quantity=>5}, #{fruit=>"banana", quantity=>100}], db:mq("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual(#{fruit=>"apple", quantity=>5},  db:mfr("select fruit, quantity from fruit order by fruit")),
     ?_assertEqual(["apple", "banana"], db:ffl("select fruit from fruit order by fruit")),
     ?_assert(db:qexists(["select * from fruit where fruit=",?P1], [banana])),
     ?_assert(db:qexists(["select * from fruit where fruit=",?P1], ["apple"])),
     ?_assertNot(db:qexists(["select * from fruit where fruit=",?P1], [<<"watermelon">>])),
     ?_assertEqual(#{fruit=>"orange", quantity=>5, description=>"oranges are orange"}, update_apple_to_orange()),
     ?_assertEqual("berry", test_insert_id()),
     ?_assertEqual(1, db:delete(fruit, fruit, "orange")),
     ?_assert(test_exists("banana")),
     ?_assertNot(db:exists(fruit, fruit, "banana-fake")),
     ?_assertNot(test_id_delete()),
     ?_assert(test_string("ﻦﺤﻧ ﺫﺎﻬﺑﻮﻧ ﻒﻳ ﺡﺎﺟﺓ ﺈﻟﻯ ﻕﺍﺮﺑ ﺄﻜﺑﺭ")),
     ?_assert(test_string("我们将需要更大的船")),
     ?_assert(test_string("Budeme potřebovat větší loď")),
     ?_assert(test_string("ჩვენ ვაპირებთ, რომ უნდა დიდი ნავი")),
     ?_assert(test_string("Мы собираемся нуждаться в большей лодку")),
     ?_assert(test_string("testy'pants")),
     ?_assert(test_string("'+\"!@#$%^&*()\\//\\//';[]<>./-=-=+")),
     ?_assert(test_encode_list(["'+\"", "!@#$'^&%", "//\\//\\", "blah","123","-=--=-!+'\"'''''''''''''''''"])),
     ?_assertEqual({some, crazy,"term"}, db:decode64(db:encode64({some, crazy,"term"}))),
     ?_assertMatch([_, _], db:q("select * from fruit " ++ db:limit_clause(2, 1))),
     ?_assertMatch([_, _], db:q("select * from fruit " ++ db:limit_clause(2, -1))),
     ?_assertMatch([_], db:q("select * from fruit " ++ db:limit_clause(-123, 5))),
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
    ID = db:pl(other, [{otherid, 0}, {my_decimal, 123.5}]),
    db:field(other, my_date, ID).

test_in_out_other(Field, V) ->
    ID = db:pl(other, [{Field, V}]),
    db:field(other, Field, ID).

test_float(Val) ->
    Fruitid = db:pl(fruit, [{some_float, Val}]),
    db:field(fruit, some_float, Fruitid).

update_apple_to_orange() ->
    Fruitid = db:fffr(["Select fruitid from fruit where fruit=",?P1], ["apple"]),
    New = [
        {fruitid, Fruitid},
        {fruit, "orange"},
        {description, "oranges are orange"}
    ],
    db:pl(fruit, New),
    db:mfr(["Select fruit, quantity, description from fruit where fruitid=", ?P1], [Fruitid]).

test_insert_id() ->
    Fruitid = db:qi("insert into fruit(fruit, quantity) values('berry', 200)"),
    db:field(fruit, fruit, Fruitid).

test_exists(Fruit) ->
    Fruitid = db:fffr(["select fruitid from fruit where fruit=",?P1], [Fruit]),
    db:exists(fruit, Fruitid).

test_id_delete() ->
    Fruitid = db:fffr(["select fruitid from fruit where fruit=",?P1], ["banana"]),
    db:delete(fruit, Fruitid),
    db:exists(fruit, Fruitid).

test_string(Str) ->
    Fruitid = db:pl(fruit, [{fruitid, 0}, {fruit, "new"}, {description, Str}]),
    Str == db:field(fruit, description, Fruitid).

test_encode_list(List) ->
    Fruitids = lists:map(fun(Fruit) ->
        db:pl(fruit, [{fruit, Fruit}])
    end, List),
    Fruitids = db:ffl(["select fruitid from fruit where fruitid in (",db:encode_list(Fruitids),") order by fruitid"]),
    Fruitids = db:ffl(["select fruitid from fruit where fruit in (", db:encode_list(List), ") order by fruitid"]),
    true.


