-module(sql_bridge_random_key_generator).
-export([generate/1, generate/2]).
-behavior(sql_bridge_key_generator).

generate(Table) ->
    Field = sql_bridge:primary_key(Table),
    generate(Table, Field).

generate(Table, Field) ->
    io:format("(generate) Primary Key: ~p -> ~p~n", [Table, Field]),
    ID = case sql_bridge:field_type(Table, Field) of
        {uuid, _} ->
            auto_increment_uuid();
        {text, MaxLength} ->
            auto_increment_string(MaxLength);
        {integer, {Min, Max}} ->
            auto_increment_integer(Min, Max)
    end,
    io:format("Auto-Generated ID: ~p~n",[ID]),
    case sql_bridge:exists(Table, Field, ID) of
        true -> generate(Table, Field);
        false -> ID
    end.

auto_increment_uuid() ->
    uuid:uuid_to_string(uuid:get_v4_urandom()).

auto_increment_string(MaxLength) ->
    [rand_char() || _ <- lists:seq(1, MaxLength)].

rand_char() ->
    case rand:uniform(62) of
        10 -> $0;
        X when X < 10 ->
            X + $0;
        X when X =< 36 ->
            X - 11 + $a;
        X ->
            X - 37 + $A
    end.

auto_increment_integer(Min, Max) ->
    Diff = Max - Min,
    rand:uniform(Diff) + Min.
