-module(sql_bridge_epgsql_codec_numeric).
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-define(MAIN_MOD, epgsql_decimal).

init(State, Sock) ->
    ?MAIN_MOD:init(State, Sock).

names() ->
    ?MAIN_MOD:names().

encode(Data, Type, State) ->
    ?MAIN_MOD:encode(Data, Type, State).

decode(Data, Type, State) ->
    Decimal = ?MAIN_MOD:decode(Data, Type, State),
    case Decimal of
        {N, 0} -> N;
        _ -> binary_to_float(decimal_conv:to_binary(Decimal, #{pretty=>false}))
    end.

decode_text(Data, Type, State) ->
    ?MAIN_MOD:decode_text(Data, Type, State).
