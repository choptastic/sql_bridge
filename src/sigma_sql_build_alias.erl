-module(sigma_sql_build_alias).
-export([build/1]).
-include_lib("syntax_tools/include/merl.hrl").

%% sigma_sql used to have its main module be called 'db', but obviously, there
%% are chances that there can be overlap there.  This script will read the
%% application var for module_alias and generate a passthru module specifically
%% for it.  By default, the module is called 'db', but changing this value will
%% allow you to specify any valid module name.  For example: sql:q() instead of
%% db:q().

build(Alias) ->
	Exports = sigma_sql:module_info(exports),
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
	FunCall ++ " -> sigma_sql:" ++ FunCall ++ ".\n".
	
arglist(Argc) ->
	Args = lists:seq($A, $A+Argc-1),
	StrArgs = [[Arg] || Arg <- Args],
	string:join(StrArgs, ",").
