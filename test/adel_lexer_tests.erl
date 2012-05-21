-module(adel_lexer_tests).
-include_lib("eunit/include/eunit.hrl").

check(Str, Expected) when is_list(Expected) ->
    Result = adel:get_tokens(Str),
    ?assertEqual({ok, Expected}, Result);

check(Str, Expected) ->
    check(Str, [Expected]).

check_identifier(Name) ->
    check(Name, {identifier, 1, list_to_atom(Name)}).

check_number(Value) ->
    check(Value, {integer, 1, list_to_integer(Value)}).

check_float(Value) ->
    check(Value, {float, 1, list_to_float(Value)}).

identifier_test() ->
    check_identifier("a"),
    check_identifier("asd"),
    check_identifier("A"),
    check_identifier("Asd"),
    check_identifier("ASD"),
    check_identifier("aSd"),
    check_identifier("a1"),
    check_identifier("a-b"),
    check_identifier("a0-B").

number_test() ->
    check_number("0"),
    check_number("1"),
    check_number("10"),
    check_number("120").

float_test() ->
    check_float("0.1"),
    check_float("1.2"),
    check_float("10.4"),
    check_float("120.55").

bool_test() ->
    check("true", {bool, 1, true}),
    check("false", {bool, 1, false}).

parens_test() ->
    check("()", [{open, 1, '('}, {close, 1, ')'}]).

block_test() ->
    check("{}", [{open_block, 1, '{'}, {close_block, 1, '}'}]).

string_test() ->
    check("\"\"", {string, 1, ""}),
    check("\"asd\"", {string, 1, "asd"}).

tag_test() ->
    check("@page(title: \"hello\", encoding: \"utf-8\") { \"hi\" }",
          [{tagprefix, 1, '@'},
           {identifier, 1, 'page'},
           {open, 1, '('},
           {identifier, 1, 'title'},
           {colon, 1, ':'},
           {string, 1, "hello"},
           {sep, 1, ','},
           {identifier, 1, 'encoding'},
           {colon, 1, ':'},
           {string, 1, "utf-8"},
           {close, 1, ')'},
           {white, 1, " "},
           {open_block, 1, '{'},
           {white, 1, " "},
           {string, 1, "hi"},
           {white, 1, " "},
           {close_block, 1, '}'}]).

clear_white_between_identifier_and_open_paren_test() ->
    Expected = [{identifier, 1, foo}, {open, 1, '('}],
    ExpectedNewLine = [{identifier, 1, foo}, {open, 2, '('}],
    check("foo (", Expected),
    check("foo\t(", Expected),
    check("foo\n(", ExpectedNewLine),
    check("foo \t (", Expected),
    check("foo    \t    \n (", ExpectedNewLine).

clear_white_between_identifier_and_open_block_test() ->
    Expected = [{identifier, 1, foo}, {open_block, 1, '{'}],
    ExpectedNewLine = [{identifier, 1, foo}, {open_block, 2, '{'}],
    check("foo {", Expected),
    check("foo\t{", Expected),
    check("foo\n{", ExpectedNewLine),
    check("foo \t {", Expected),
    check("foo    \t    \n {", ExpectedNewLine).

clear_white_after_open_paren_test() ->
    Expected = [{open, 1, '('}, {identifier, 1, foo}],
    check("(      foo", Expected),
    check("( foo", Expected).

clear_white_before_close_paren_test() ->
    Expected = [{close, 1, ')'}],
    check(" )", Expected),
    check("     )", Expected).

clear_white_around_colon_test() ->
    Expected = [{colon, 1, ':'}],
    check(" :", Expected),
    check(": ", Expected),
    check(" : ", Expected),
    check(":     ", Expected),
    check("     :", Expected),
    check("     :        ", Expected).

clear_white_around_sep_test() ->
    Expected = [{sep, 1, ','}],
    check(" ,", Expected),
    check(", ", Expected),
    check(" , ", Expected),
    check(",     ", Expected),
    check("     ,", Expected),
    check("     ,        ", Expected).

