-module(adel).

-export([get_tokens/1, get_ast/1, run/1, to_xmerl/1, to_xml/1, expand/1]).

get_tokens(String) ->
    case adel_lexer:string(String) of
        {ok, Tokens, _EndLine} ->
            {ok, clean_whites(Tokens)};
        Errors ->
            {false, Errors}
    end.

clean_whites(Tokens) ->
    clean_whites(Tokens, []).

clean_whites([], Acc) ->
    lists:reverse(Acc);

% remove spaces after identifier and before open parenthesis
clean_whites([{identifier, _, _}=Token, {white, _, _}, {open, _, _}=Token1|T], Acc) ->
    clean_whites([Token1|T], [Token|Acc]);

% remove spaces after identifier and before open block
clean_whites([{identifier, _, _}=Token, {white, _, _}, {open_block, _, _}=Token1|T], Acc) ->
    clean_whites([Token1|T], [Token|Acc]);

% remove spaces after open paren
clean_whites([{open, _, _}=Token, {white, _, _}|T], Acc) ->
    clean_whites([Token|T], Acc);

% remove spaces before close paren
clean_whites([{white, _, _}, {close, _, _}=Token|T], Acc) ->
    clean_whites([Token|T], Acc);

% remove spaces after colon
clean_whites([{colon, _, _}=Token, {white, _, _}|T], Acc) ->
    clean_whites([Token|T], Acc);

% remove spaces before colon
clean_whites([{white, _, _}, {colon, _, _}=Token|T], Acc) ->
    clean_whites([Token|T], Acc);

% remove spaces after sep
clean_whites([{sep, _, _}=Token, {white, _, _}|T], Acc) ->
    clean_whites([Token|T], Acc);

% remove spaces before sep
clean_whites([{white, _, _}, {sep, _, _}=Token|T], Acc) ->
    clean_whites([Token|T], Acc);

clean_whites([Token|T], Acc) ->
    clean_whites(T, [Token|Acc]).


get_ast(String) ->
    {ok, Tokens} = get_tokens(String),

    case adel_parser:parse(Tokens) of
        {ok, Tree}                        -> {ok, Tree, []};
        {ok, _Tree, _}=Result             -> Result;
        {error, _Warnings, _Errors}=Errors -> Errors
    end.

file_to_string(Path) ->
    Content = case file:read_file(Path) of
        {ok, Return} -> Return;
        {error, _Reason} = Error -> throw(Error)
    end,

    binary_to_list(Content).

expand(Ast) ->
    expand(Ast, []).

expand([], Accum) ->
    lists:reverse(Accum);

expand([{cons, _First, _Rest}=Expr|T], Accum) ->
    Result = adel_funs:eval(Expr),

    expand(T, [Result|Accum]);

expand([{tag, Name, Attrs, Body}|T], Accum) ->
    Result = {tag, Name, Attrs, expand(Body)},

    expand(T, [Result|Accum]);

expand([H|T], Accum) ->
    expand(T, [H|Accum]).

to_xmerl_single({tag, '', Attrs, Childs}) ->
    {'div', to_xmerl(Attrs), to_xmerl(Childs)};

to_xmerl_single({tag, '!white', _Attrs, Data}) ->
    Data;

to_xmerl_single({tag, '!string', _Attrs, Data}) ->
    Data;

% TODO: don't know if this nodes should reach this place in this
% form
to_xmerl_single({string, _Line, Data}) ->
    Data;

to_xmerl_single({integer, _Line, Data}) ->
    integer_to_list(Data);

to_xmerl_single({float, _Line, Data}) ->
    float_to_list(Data);

to_xmerl_single({bool, _Line, true}) ->
    "true";

to_xmerl_single({bool, _Line, false}) ->
    "false";

% /TODO

to_xmerl_single({tag, Name, Attrs, Childs}) ->
    {Name, to_xmerl(Attrs), to_xmerl(Childs)};

to_xmerl_single({attr, Name, Value}) when is_list(Value) ->
    {Name, Value};

to_xmerl_single({attr, Name, Value}) when is_integer(Value) ->
    {Name, integer_to_list(Value)};

to_xmerl_single({attr, Name, Value}) when is_float(Value) ->
    {Name, float_to_list(Value)};

to_xmerl_single({attr, Name, true}) ->
    {Name, "true"};

to_xmerl_single({attr, Name, false}) ->
    {Name, "false"}.

to_xmerl(Ast) ->
    to_xmerl(Ast, []).

to_xmerl([], Accum) ->
    lists:reverse(Accum);

to_xmerl([H|T], Accum) ->
    to_xmerl(T, [to_xmerl_single(H)|Accum]).

to_xml(Doc) ->
    xmerl:export_simple(Doc, xmerl_xml).

run(["tokens", Path]) ->
    io:format("~p~n", [get_tokens(file_to_string(Path))]);

run(["ast", Path]) ->
    io:format("~p~n", [get_ast(file_to_string(Path))]);

run(["expand", Path]) ->
    Data = file_to_string(Path),
    {ok, Ast, _} = get_ast(Data),
    Expanded = expand(Ast),

    io:format("~p~n", [Expanded]);

run(["xmerl", Path]) ->
    Data = file_to_string(Path),
    {ok, Ast, _} = get_ast(Data),
    Expanded = expand(Ast),
    Xmerl = to_xmerl(Expanded),

    io:format("~p~n", [Xmerl]);

run(["xml", Path]) ->
    Data = file_to_string(Path),
    {ok, Ast, _} = get_ast(Data),
    Expanded = expand(Ast),
    Xmerl = to_xmerl(Expanded),
    Xml = to_xml(Xmerl),

    io:format("~s~n", [Xml]).
