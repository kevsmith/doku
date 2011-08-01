-module(doku_renderer).

-include("doku.hrl").

-export([render/3]).

render(Comments, Code, Output0) ->
    {ok, Css} = doku_pygmentize:default_css(),
    Output = filename:absname(Output0),
    OutBuf = "<html><head><style type=\"text/css\">" ++
             Css ++ "td{vertical-align: top;}" ++
             "col.comment{background-color: WhiteSmoke;}" ++
             "td.comment{font-family: helvetica; font-size: small;padding: 0px 10px 0px 0px;}" ++
             "td.empty{padding: 0px 0px 0px 0px;}" ++
             "td.code{padding: 0px 0px 0px 0x;}" ++
             "table{border-collapse: collapse;}" ++
            "</style><body><table><col class=\"comment\" span=\"1\"></col><col span=\"1\"></col>",
    EndLine = find_end(Comments, Code),
    OutBuf1 = generate_content(Comments, Code, 1, EndLine, OutBuf),
    OutBuf2 = OutBuf1 ++ "</table></body></html>",
    io:format("Writing to ~p~n", [Output]),
    file:write_file(Output, iolist_to_binary(OutBuf2)).

find_end(Comments, Code) ->
    MaxCommentLine = find_max(Comments),
    MaxCodeLine = find_max(Code),
    case MaxCommentLine > MaxCodeLine of
        true ->
            MaxCommentLine;
        false ->
            MaxCodeLine
    end.

find_max([]) ->
    0;
find_max(Entities) ->
    {_, _, End, _} = hd(lists:reverse(Entities)),
    End.

generate_content(Comments, Code, Line, End, Buf) when Line =< End ->
    Buf1 = Buf ++ "<tr>\n",
    {Comments1, Buf2} = render_comment(Line, Comments, Buf1),
    {Code1, Buf3} = render_code(Line, Code, Buf2),
    Buf4 = Buf3 ++ "</tr>\n",
    generate_content(Comments1, Code1, Line + 1, End, Buf4);
generate_content(_, _, _, _, Buf) ->
    Buf.

render_comment(Line, Comments, Buf) ->
    case lists:keytake(Line, 2, Comments) of
        false ->
            {Comments, Buf ++ "<td class=\"empty\"></td>\n"};
        {value, #comment{text=Text}, Comments1} ->
            {Comments1, Buf ++ "<td class=\"comment\">" ++ Text ++ "</td>\n"}
    end.

render_code(Line, Code, Buf) ->
    case lists:keytake(Line, 2, Code) of
        false ->
            {Code, Buf ++ "<td class=\"empty\"></td>\n"};
        {value, #form{text=Text}, Code1} ->
            {Code1, Buf ++ "<td class=\"code\">" ++ Text ++ "</td>\n"}
    end.
