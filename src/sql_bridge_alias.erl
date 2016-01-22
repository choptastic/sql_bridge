-module(sql_bridge_alias).
-export([build/1]).
-export([build_stringify/1]).
-include_lib("syntax_tools/include/merl.hrl").

build_stringify(Stringify) ->
	stringify_msg(Stringify),
	Modtext = [
		build_header(sql_bridge_stringify),
		build_maybe_string(Stringify)
	],
	Forms = merl:quote(Modtext),
	Res = merl:compile_and_load(Forms),
	case Res of
		{ok, _} -> ok;
		error ->
			error_logger:error_msg("Unable to compile alias module (~p).~n~s~n", [Modtext]),
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
	

%% sql_bridge used to have its main module be called 'db', but obviously, there
%% are chances that there can be overlap there.  This script will read the
%% application var for module_alias and generate a passthru module specifically
%% for it.  By default, the module is called 'db', but changing this value will
%% allow you to specify any valid module name.  For example: sql:q() instead of
%% db:q().

build(Alias) ->
	msg(io_lib:format("Building Alias module (~p) to sql_bridge.",[Alias])),
	Exports = sql_bridge:module_info(exports),
	Modtext = build_module(Alias, Exports),
	Forms = merl:quote(Modtext),
	Res = merl:compile_and_load(Forms),
	case Res of 
		{ok, _} -> ok;
		error ->
			error_logger:error_msg("Unable to compile alias module (~p).~n~s~n", [Modtext]),
			error
	end.

build_module(Alias, Exports) ->
	lists:flatten([
		build_header(Alias),
		build_exports(Exports)
	]).

build_header(Alias) ->
	"-module(" ++ atom_to_list(Alias) ++ ").\n"
	"-compile(export_all).\n".

build_exports(Exports) ->
	[build_export(FunArity) || FunArity <- Exports].

build_export({module_info,_}) ->
	""; %% ignore module_info
build_export({Fun, Arity}) ->
	Arglist = arglist(Arity),
	StrFun = atom_to_list(Fun),
	FunCall = StrFun ++ "(" ++ Arglist ++ ")",
	_Line = prefixize(FunCall).

prefixize(FunCall) ->
	FunCall ++ " -> sql_bridge:" ++ FunCall ++ ".\n".
	
arglist( Argc) ->
	Args = lists:seq($A, $A+Argc-1),
	StrArgs = [[Arg] || Arg <- Args],
	string:join(StrArgs, ",").

msg(Msg) ->
	io:format("SQL Bridge: ~s~n",[Msg]).
