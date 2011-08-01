-module(doku_parser).

-include("doku.hrl").

-export([analyze/1]).

analyze(FileName) ->
    case clean_parse(FileName) of
        {ok, Comments, Tree} ->
            Comments1 = format_comment(Comments, []),
            Forms = format_forms(Tree, []),
            Comments2 = adjust_comments(Comments1, Forms, []),
            {ok, Comments2, Forms};
        Error ->
            doku_log:fatal("Error processing ~s: ~p~n", [FileName, Error])
    end.

format_forms([], Accum) ->
    lists:keysort(2, Accum);
format_forms([{tree, _, _, _}=Form|T], Accum) ->
    Start = tree_lineno(Form),
    FForm = format_form(Start, Form),
    format_forms(T, [FForm|Accum]);
format_forms([_Form|T], Accum) ->
    format_forms(T, Accum).

format_form(Start, Form) ->
    FForm = erl_prettypr:format(Form),
    {ok, Tmp} = doku_tempfile:make(),
    file:write_file(Tmp, FForm),
    {ok, FForm1} = doku_pygmentize:highlight_file(Tmp),
    file:delete(Tmp),
    Size = case doku_util:char_count($\n, FForm) of
               0 ->
                   1;
               S ->
                   S - 1
           end,
    #form{start=Start, finish=Start + Size, text=FForm1}.

format_comment([], Accum) ->
    lists:keysort(2, Accum);
format_comment([{Line, _, _, Comments0}|T], Accum) ->
    F = fun(C) -> re:replace(C, "^(%)* |^(%)*", "", [{return, list}]) end,
    Comments1 = [F(C) || C <- Comments0],
    Text = string:join(Comments1, "\n"),
    Text1 = markdown:conv(Text),
    Comments2 = #comment{start=Line, finish=Line + length(Comments1) - 1,
                         text=Text1},
    format_comment(T, [Comments2|Accum]).

clean_parse(FileName) ->
    {ok, Tmp} = doku_tempfile:make(),
    Comments = erl_comment_scan:file(FileName),
    case epp_dodger:parse_file(FileName) of
        {ok, Tree0} ->
            Tree1 = {tree, form_list, {attr, 0, [], none}, Tree0},
            file:write_file(Tmp, [erl_prettypr:format(Tree1)]),
            {ok, Comments, Tree0};
        Error ->
            doku_log:fatal("Error processing file ~s: ~p~n", [FileName, Error])
    end.

tree_lineno({tree, _, {attr, LineNo, _, _}, _}) ->
    LineNo.

adjust_comments([], _Forms, Accum) ->
    lists:keysort(2, Accum);
adjust_comments([#comment{start=S, finish=F, text=Txt}=C|T], Forms, Accum) ->
    case lists:keyfind(F + 1, 2, Forms) of
        false ->
            adjust_comments(T, Forms, [C|Accum]);
        #form{start=FS} ->
            S1 = FS,
            F1 = S1 + (F - S),
            adjust_comments(T, Forms, [#comment{start=S1, finish=F1, text=Txt}|Accum])
    end.
