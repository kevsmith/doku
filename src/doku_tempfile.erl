-module(doku_tempfile). 
-export([make/0, make/1]). 

make() -> 
    make([{"-t", "doku"}]).

make(Args) -> 
    Cmd = process_args(Args, "mktemp"),
    Port = open_port({spawn, Cmd}, [{line, 1000}, 
                                    use_stdio, 
                                    exit_status, 
                                    stderr_to_stdout]), 
    get_response(Port, nil). 

process_args([], Cmd) ->
    Cmd;
process_args([{Opt, Arg}|T], Cmd) ->
    Cmd1 = Cmd ++ " " ++ Opt ++ " " ++ Arg,
    process_args(T, Cmd1).


get_response(Port, Resp) -> 
    case Resp of 
        nil -> 
            receive 
                { Port, {data, {_, Line}}} -> 
                    get_response(Port, Line); 
                { Port, {exit_status, _ }} -> 
                    { error, "No response from mktemp" } 
            end; 
        Resp -> 
            receive 
                { Port, {data, _}} -> 
                    get_response(Port, Resp); 
                { Port, {exit_status, 0}} -> 
                    { ok, Resp }; 
                { Port, {exit_status, _}} -> 
                    { error, Resp } 
            end 
    end.
