#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

%% Erlang 17 Changed the expected types of dict(), gb_trees(), etc to require
%% prefixing with the module name. This caused a non-backwards compatibile
%% break with versions prior to Erlang 17.
%%
%% This script will check the Erlang version and if otp_release >= 17, it will create
%% custom types called emysql_dict(), etc which just refer to dict:dict(), making the
%% types fully backwards compatible, and if OTP prior to 17, will create
%% emysql_dict() which refers to dict().
%%
%% It will do this for dict(), queue(), and gb_tree()
%%
%% Note: This should be run *before* compiling!

-define(TYPE_PREFIX, "t_").
-define(TYPES, [
   %{old version, new version}
    {"dict()", "dict:dict()"},
    {"queue()", "queue:queue()"},
    {"gb_tree()", "gb_trees:tree()"}
]).

main([]) ->
    crypto:start(),

	Filename = "include/compat.hrl",
	io:format("Generating ~p ...~n", [Filename]),

    TypeStrings = process_otp_version(erlang:system_info(otp_release)),

	Contents = [
        "%% Note: This file was automatically generated. Do not include it in source control\n",
        TypeStrings
	],

    ContentsBin = iolist_to_binary(Contents),
    case file:read_file(Filename) of
        {ok, ContentsBin} -> 
            io:format("...no changes needed to ~p. Skipping writing new file~n", [Filename]);
        _ -> 
            io:format("...writing ~p~n", [Filename]),
            file:write_file(Filename, Contents)
    end.

process_otp_version(Version = "R" ++ _) ->
    io:format("...OTP ~s does not require module prefixes for types...~n",[Version]),
    [make_type_pre_17(OldType) || {OldType, _} <- ?TYPES];
process_otp_version(Version) ->
    io:format("...OTP ~s requires module prefixes for types...~n",[Version]),
    [make_type_17_plus(Types) || Types <- ?TYPES].

make_type_pre_17(Type) ->
    ["-type ",?TYPE_PREFIX,Type, " :: ",Type,".\n"].

make_type_17_plus({Type, FullType}) ->
    ["-type ",?TYPE_PREFIX,Type, " :: ",FullType,".\n"].
