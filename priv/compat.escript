#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

%% NOTE: this file no longer needs to be run, at least for now.
%% I'm keeping it here in case, future compat changes are necessary, and it can
%% be used as a basis for the future scripts.
%%
%% IF WE DO NEED IT, we'll need to remove the `compat.hrl` file from source
%% control again.
%%

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

main([]) ->
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

process_otp_version(Version) ->
    io:format("sql_bridge compat: OTP ~s:~n", [Version]),
    %io:format("      + requires module prefixes for types~n"),
    %io:format("      + supports maps...~n"),
    [
    %    [make_type_17_plus(Types) || Types <- ?TYPES],
    %    make_map_macro()
    ].

%make_type_pre_17(Type) ->
%    ["-type ",?TYPE_PREFIX,Type, " :: ",Type,".\n"].
%
%make_type_17_plus({Type, FullType}) ->
%    ["-type ",?TYPE_PREFIX,Type, " :: ",FullType,".\n"].
%
%make_map_macro() ->
%    ["-define(has_maps, true).\n"].
%
%make_nomap_macro() ->
%    [].
