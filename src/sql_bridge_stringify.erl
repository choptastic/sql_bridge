%% This module will be dynamically generated immediately, and will never
%% actually be run exactly as-is. Instead, the `sql_bridge_alias` module will
%% build a new one on the fly based on the configuration.
%%
%% This module as is, currently only exists to satisfy dialyzer for being an
%% unknown module.
%%
%% Further, it has an `-on_load` attribute to force the module to self-rebuild
%% in the event of a release upgrade so that the behavior changes as needed.

-module(sql_bridge_stringify).
-export([maybe_string/1]).
-on_load(rebuild/0).

-spec maybe_string(null | binary() | list()) -> undefined | binary() | list().
maybe_string(null) -> undefined;
maybe_string(B) -> B.

rebuild() ->
    sql_bridge:build_stringify().
