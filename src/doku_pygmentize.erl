-module(doku_pygmentize).

-export([default_css/0,
         highlight_file/1]).

-define(PYG, "pygmentize").

default_css() ->
    Cmd = ?PYG ++ " -S emacs -f html",
    run_cmd(Cmd).

highlight_file(FileName) ->
    Cmd = ?PYG ++ " -l erlang -f html -O emacs " ++ FileName,
    run_cmd(Cmd).

run_cmd(Cmd) ->
    Port = open_port({spawn, Cmd}, [{line, 5000},
                                    use_stdio, exit_status, stderr_to_stdout]),
    read_response(Port, []).

read_response(Port, Resp) ->
    receive
        {Port, {data, {_, Data}}} ->
            read_response(Port, [Data|Resp]);
        {_Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Resp))};
         {_Port, {exit_status, _Status}} ->
            error
    end.
