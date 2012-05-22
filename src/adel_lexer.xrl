Definitions.

True       = true
False      = false
Identifier = [A-Za-z][A-Za-z\-0-9]*

% numbers
Number      = [0-9]
Float       = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?

Open        = \(
Close       = \)
OpenBlock   = {
CloseBlock  = }
Sep         = ,
Whites      = (\s|\t|\n)+
Dot         = \.
Colon       = :

% string stuff
String      = "(\\\^.|\\.|[^\"])*"

Rules.

% numbers
{Float}                  : make_token(float,   TokenLine, TokenChars, fun erlang:list_to_float/1).
{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).

% delimiters and operators
{Open}                   : make_token(open,        TokenLine, TokenChars).
{Close}                  : make_token(close,       TokenLine, TokenChars).
{OpenBlock}              : make_token(open_block,  TokenLine, TokenChars).
{CloseBlock}             : make_token(close_block, TokenLine, TokenChars).

{Sep}                    : make_token(sep,          TokenLine, TokenChars).
{Dot}                    : make_token(dot,          TokenLine, TokenChars).
{Colon}                  : make_token(colon,        TokenLine, TokenChars).

% string stuff
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).

% identifiers and atoms
{True}                   : make_token(bool, TokenLine, true).
{False}                  : make_token(bool, TokenLine, false).
{Identifier}             : make_token(identifier, TokenLine, TokenChars).
@                        : make_token(tagprefix,  TokenLine, TokenChars).
\$                       : make_token(varprefix,  TokenLine, TokenChars).

% spaces, tabs and new lines
{Whites}                 : {token, {white, TokenLine, TokenChars}}.

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.
