-module(doku).

-export([main/1]).

-include("doku.hrl").

-define(OPTIONS,
        [{file, $f, "file", {string, ""}, "File to process"},
         {dir, $d, "dir", {string, ""}, "Directory to process"},
         {out, $o, "out", {string, "./doku"}, "Output directory"},
         {help, $h, "help", undefined, "Display brief help"}
        ]).

-define(FILE_ATTR(FileName), {attribute, 1, file, {FileName, 1}}).

main(Args) ->
    %% Verify all external tools are avaialble
    find_all_tools([{"pygmentize", "pygmentize is required to generate syntax highlights"}]),
    %% Parse command line options
    {ok, {Opts, _}} = getopt:parse(?OPTIONS, Args),
    %% Display help and exit if user assked for help
    display_help(Opts),
    %% Process files and generate output
    File = proplists:get_value(file, Opts),
    Dir = proplists:get_value(dir, Opts),
    Out = proplists:get_value(out, Opts),
    {ok, Comments, Code} = process_files(File, Dir),
    doku_renderer:render(Comments, Code, Out).

%% Internal functions
find_all_tools([]) ->
    ok;
find_all_tools([{Cmd, Error}|T]) ->
    case doku_util:is_on_path(Cmd) of
        false ->
            doku_log:fatal("~s~n", [Error]);
        true ->
            find_all_tools(T)
    end.

display_help(Opts) ->
    case lists:member(help, Opts) of
        true ->
            getopt:usage(?OPTIONS, atom_to_list(?MODULE)),
            erlang:halt(0);
        false ->
            false
    end.

process_files(File, Dir) when File == Dir ->
    doku_log:fatal("Either a file or a directory MUST be specified");

process_files(File, "") ->
    doku_parser:analyze(File).
