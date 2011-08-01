-module(doku_log).

-export([fatal/1,
         fatal/2]).

fatal(Msg) ->
    io:format("~s~n", [Msg]),
    erlang:halt(1).

fatal(Format, Msg) ->
    io:format(Format, Msg),
    erlang:halt(1).
