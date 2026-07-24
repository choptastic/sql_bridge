-module(doc_attr_compat).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    try list_to_integer(erlang:system_info(otp_release)) of
        X when X < 27 ->
            remove_docs(Forms);
        _ ->
            Forms
    catch _:_ ->
        remove_docs(Forms)
    end.

remove_docs(Forms) ->
    io:format("Removing -doc attributes for Erlang version < 27~n"),
    [F || F <- Forms, not(is_doc(F))].

is_doc({attribute, _, doc, _}) ->
    true;
is_doc(_) ->
    false.
