-module(sql_bridge_alias).
-export([build/1]).
-export([build_stringify/1]).
-include_lib("syntax_tools/include/merl.hrl").


%% sql_bridge used to have its main module be called 'db', but obviously, there
%% are chances that there can be overlap there.  This script will read the
%% application var for module_alias and generate a passthru module specifically
%% for it.  By default, the module is called 'db', but changing this value will
%% allow you to specify any valid module name.  For example: sql:q() instead of
%% db:q().
%% The aliasing functionality has been migrated to a new library called `erlias`
%% https://github.com/choptastic/erlias

build(Alias) ->
    erlias:build(sql_bridge, Alias).


%% We still build a separate module just for automatic binary->string conversion
%% If the configuration calls for it.

build_stringify(Stringify) ->
	stringify_msg(Stringify),
	Modtext = [
        "-module(sql_bridge_stringify).\n",
        "-compile(export_all).\n",
		build_maybe_string(Stringify)
	],
	Forms = merl:quote(Modtext),
	Res = merl:compile_and_load(Forms),
	case Res of
		{ok, _} -> ok;
		error ->
			error_logger:error_msg("Unable to build module (sql_bridge_stringify).~n~s~n", [Modtext]),
			error
	end.

build_maybe_string(false) ->
	"maybe_string(null) -> undefined;
	 maybe_string(B) -> B.";
build_maybe_string(true) ->
	"maybe_string(null) -> undefined;
	 maybe_string(B) -> sql_bridge_utils:binary_to_string(B).".

stringify_msg_base() ->
	"Building sql_bridge_stringify.".

stringify_msg(true) ->
	Base = stringify_msg_base(),
	Rest = "Binaries WILL automatically be converted to lists in query responses.",
	msg(Base ++ " " ++ Rest);
stringify_msg(false) ->
	Base = stringify_msg_base(),
	Rest = "Binaries WILL NOT automatically be converted to lists in query responses.",
	msg(Base ++ " " ++ Rest).
	


msg(Msg) ->
	io:format("SQL Bridge: ~s~n",[Msg]).
