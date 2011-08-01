-module(doku_util).

-export([is_on_path/1,
         char_count/2]).

is_on_path(Cmd) ->
    PathVar = os:getenv("PATH"),
    Paths = string:tokens(PathVar, ":"),
    search_paths(Paths, Cmd).

char_count(Char, Text) ->
    F = fun(C, Acc) ->case C of
                          Char -> Acc + 1;
                          _ -> Acc
                      end end,
    lists:foldl(F, 0, Text).

search_paths([], _Cmd) ->
    false;
search_paths([Path|T], Cmd) ->
    CmdPath = filename:join([Path, Cmd]),
    case filelib:is_file(CmdPath) of
        true ->
            true;
        false ->
            search_paths(T, Cmd)
    end.

