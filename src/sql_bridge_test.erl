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

mysql_otp_test_() ->
	{setup,
	 fun() -> gen_setup(sql_bridge_mysql_otp, postgres, 3306) end,
	 fun mysql_otp_cleanup/1,
	 fun main_tests/1
	}.

epgsql_otp_test_() ->
	{setup,
	 fun() -> gen_setup(sql_bridge_epgsql, mysql, 5432) end,
	 fun epgsql_cleanup/1,
	 fun main_tests/1
	}.

emysql_otp_test_() ->
	{setup,
	 fun() -> gen_setup(sql_bridge_emysql, mysql, 3306) end,
	 fun epgsql_cleanup/1,
	 fun main_tests/1
	}.


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
	sql_bridge:start().


epgsql_cleanup(_) ->
	application:stop(epgsql),
	application:stop(sql_bridge),
	ok.

mysql_otp_cleanup(_) ->
	application:stop(mysql),
	application:stop(sql_bridge),
	ok.

emysql_cleanup(_) ->
	application:stop(emysql),
	application:stop(sql_bridge),
	ok.

main_tests(_) ->
	[
	 ?_assertMatch(_, db:q("delete from fruit")),
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

	 ?_assertMatch([fruitid, fruit, description, quantity, picture], db:table_fields(fruit)),
	 ?_assertMatch(_, db:qi(["insert into fruit(fruit, quantity) values(", ?P1, ",", ?P2, ")"], ["apple", 5])),
	 ?_assertEqual(5, db:fffr(["select quantity from fruit where fruit=",?P1 ], [apple])),
	 ?_assertEqual("apple", db:fffr(["select fruit from fruit where quantity=", ?P1], [5])),
	 ?_assertMatch(_, db:pl(fruit, [{fruitid, 0}, {fruit, <<"banana">>}, {quantity, 100}, {description, "long and yellow"}])),
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
	 ?_assert(test_string("'+\"!@#$%^&*()\\//\\//';[]<>./-=-=+"))
	].

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
