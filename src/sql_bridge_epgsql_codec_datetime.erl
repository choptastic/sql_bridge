-module(sql_bridge_epgsql_codec_datetime).
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-define(MAIN_MOD, epgsql_codec_datetime).
-define(IS_STRING(Val), (is_binary(Data) orelse is_list(Data))).

init(State, Sock) ->
    ?MAIN_MOD:init(State, Sock).

names() ->
    ?MAIN_MOD:names().

encode(Data, date=Type, State) when ?IS_STRING(Data) ->
    Data2 = s2date(Data),
    ?MAIN_MOD:encode(Data2, Type, State);
encode(Data, time=Type, State) when ?IS_STRING(Data) ->
    Data2 = s2time(Data),
    ?MAIN_MOD:encode(Data2, Type, State);
encode({Data, TZ}, timetz=Type, State) when ?IS_STRING(Data) ->
    Data2 = s2time(Data),
    ?MAIN_MOD:encode({Data2, TZ}, Type, State);
encode(Data, timestamp=Type, State) when ?IS_STRING(Data) ->
    Data2 = s2timestamp(Data),
    ?MAIN_MOD:encode(Data2, Type, State);
encode(Data, timestamptz=Type, State) when ?IS_STRING(Data) ->
    Data2 = s2timestamp(Data),
    ?MAIN_MOD:encode(Data2, Type, State);
encode(Data, Type, State) ->
    ?MAIN_MOD:encode(Data, Type, State).

s2date(Data) ->
    RE = "^(\\d{4})-(\\d{1,2})-(\\d{1,2})$",
    case re:run(Data, RE, [{capture, all_but_first, list}]) of
        nomatch ->
            exit({invalid_date_format, Data});
        {match, [Y,M,D]} ->
            {list_to_integer(Y), list_to_integer(M), list_to_integer(D)}
    end.

s2time(Data) ->
    RE = "^(\\d{1,2}):(\\d{2})(?::(\\d{2}))?$",
    case re:run(Data, RE, [{capture, all_but_first, list}]) of
        nomatch ->
            exit({invalid_time_format, Data});
        {match, [H, M]} ->
            {list_to_integer(H), list_to_integer(M), 0};
        {match, [H, M, S]} ->
            {list_to_integer(H), list_to_integer(M), list_to_integer(S)}
    end.

s2timestamp(Data) ->
    case re:split(Data, "[\\sT]", [{return, list}]) of
        [DateStr, TimeStr] ->
            Date = s2date(DateStr),
            Time = s2time(TimeStr),
            {Date, Time};
        _ ->
            exit({invalid_datetime_format, Data})
    end.

decode(Data, Type, State) ->
    ?MAIN_MOD:decode(Data, Type, State).

decode_text(Data, Type, State) ->
    ?MAIN_MOD:decode_text(Data, Type, State).

