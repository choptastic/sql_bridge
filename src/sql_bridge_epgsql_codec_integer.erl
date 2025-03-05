-module(sql_bridge_epgsql_codec_integer).
-behaviour(epgsql_codec).
-include_lib("epgsql/include/protocol.hrl").

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-define(MAIN_MOD, epgsql_codec_integer).

init(State, Sock) ->
    ?MAIN_MOD:init(State, Sock).

names() ->
    ?MAIN_MOD:names().

encode(Data, Type, State) ->
    try normalize_int(Data) of
        N ->  ?MAIN_MOD:encode(N, Type, State)
    catch _:_ -> 
        %% Hardcode the NULL term here (not sure the better way)
        logger:warning("~p: Failed to convert term (~p) to integer. Convering to 'null'", [?MODULE, Data]),
        << -1:?int32>>
    end.

decode(Data, Type, State) ->
    ?MAIN_MOD:decode(Data, Type, State).

decode_text(Data, Type, State) ->
    ?MAIN_MOD:decode_text(Data, Type, State).

normalize_int(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N));
normalize_int(N) when is_list(N) ->
    list_to_integer(N);
normalize_int(N) when is_integer(N) ->
    N.
