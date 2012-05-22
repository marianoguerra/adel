-module(adel_parser_tests).
-include_lib("eunit/include/eunit.hrl").

check(Str, Expected) when is_list(Expected) ->
    {ok, Result, _} = adel:get_ast(Str),
    ?assertEqual(Expected, Result);

check(Str, Expected) ->
    check(Str, [Expected]).

tag(Name) ->
    tag(Name, [], []).

tag(Name, Attrs) ->
    tag(Name, Attrs, []).

tag(Name, Attrs, Childs) ->
    {tag, Name, Attrs, Childs}.

attr(Name, Val) ->
    {attr, Name, Val}.

tag_test() ->
    check("@foo", tag(foo)),
    check("@foo@bar", [tag(foo), tag(bar)]),
    check("@foo()", tag(foo)),
    check("@foo(){}", tag(foo)),
    check("@foo{}", tag(foo)).

anon_tag_test() ->
    check("@()", tag('')),
    check("@(){}", tag('')),
    check("@{}", tag('')).

tag_single_attr_test() ->
    check("@user(name:\"mariano\")", tag(user, [attr(name, "mariano")])),
    check("@(name:\"mariano\")", tag('', [attr(name, "mariano")])).

tag_attrs_test() ->
    Attrs = [
        attr(name, "mariano"),
        attr(deleted, false),
        attr(age, 27),
        attr(balance, 10.2)],

    check("@user(name:\"mariano\",deleted:false,age:27,balance:10.2)",
          tag(user, Attrs)),

    check("@(name:\"mariano\",deleted:false,age:27,balance:10.2)",
          tag('', Attrs)).

tag_child_test() ->
    check("@user{@account}",
          tag(user, [], [tag(account)])),

    check("@{@account}",
          tag('', [], [tag(account)])).

tag_childs_test() ->
    check("@user{@account@address}",
          tag(user, [], [tag(account), tag(address)])),

    check("@{@account@address}",
          tag('', [], [tag(account), tag(address)])).

tag_nesteds_test() ->
    check("@user{@account{@address}}",
          tag(user, [], [tag(account, [], [tag(address)])])),

    check("@{@{@{}}}",
          tag('', [], [tag('', [], [tag('')])])).

tag_attrs_and_childs_test() ->
    Attrs = [
        attr(name, "mariano"),
        attr(deleted, false),
        attr(age, 27),
        attr(balance, 10.2)],

    Childs = [
        tag(account, [attr(user, "mariano"), attr(host, "marianoguerra.org")]),
        tag(address, [attr(name, "fakestr"), attr(number, 123)])],

    StrAttrs = "(name:\"mariano\",deleted:false,age:27,balance:10.2)",
    StrChilds = "{@account(user:\"mariano\",host:\"marianoguerra.org\")@address(name:\"fakestr\",number:123)}",

    check("@user" ++ StrAttrs ++ StrChilds,
          tag(user, Attrs, Childs)),

    check("@" ++ StrAttrs ++ StrChilds,
          tag('', Attrs, Childs)).

var_test() ->
    check("$foo", {var, foo}).

code_test() ->
    check("@(append 1 2)", {cons, {identifier, 1, append},
                            {cons, {integer, 1, 1},
                             {cons, {integer, 1, 2}, nil}}}).
