Nonterminals

doc tags tag attrs attr value attr_block body_block var bodyitem code
code_args code_value.

Terminals

tagprefix identifier open close open_block close_block string integer float
bool white sep colon varprefix.

Rootsymbol doc.

doc -> tags: '$1'.

tags -> bodyitem : ['$1'].
tags -> bodyitem tags : ['$1'|'$2'].

bodyitem -> code:   '$1'.
bodyitem -> tag:    '$1'.
bodyitem -> white:  tag('!white', [], unwrap('$1')).
bodyitem -> string: tag('!string', [], unwrap('$1')).
bodyitem -> var:    '$1'.

code -> tagprefix open code_args close: '$3'.

code_args -> code_value: {cons, '$1', nil}.
code_args -> code_value white code_args: {cons, '$1', '$3'}.

tag -> tagprefix identifier : tag(unwrap('$2')).
tag -> tagprefix identifier attr_block: tag(unwrap('$2'), '$3').
tag -> tagprefix identifier body_block: tag(unwrap('$2'), [], '$3').
tag -> tagprefix identifier attr_block body_block: tag(unwrap('$2'), '$3', '$4').

tag -> tagprefix attr_block: tag('', '$2').
tag -> tagprefix body_block: tag('', [], '$2').
tag -> tagprefix attr_block body_block: tag('', '$2', '$3').

attr_block -> open close: [].
attr_block -> open attrs close: '$2'.

body_block -> open_block close_block: [].
body_block -> open_block tags close_block: '$2'.

attrs -> attr : ['$1'].
attrs -> attr sep attrs : ['$1'|'$3'].

attr -> identifier colon value: attr(unwrap('$1'), unwrap('$3')).

code_value -> value: '$1'.
code_value -> tag: '$1'.
code_value -> identifier: '$1'.

value -> integer: '$1'.
value -> float:   '$1'.
value -> string:  '$1'.
value -> bool:    '$1'.
value -> var:     '$1'.
value -> code:    '$1'.

var -> varprefix identifier: var(unwrap('$2')).

Erlang code.

tag(Name) ->
    tag(Name, [], []).

tag(Name, Attrs) ->
    tag(Name, Attrs, []).

tag(Name, Attrs, Childs) ->
    {tag, Name, Attrs, Childs}.

attr(Name, Val) ->
    {attr, Name, Val}.

var(Name) ->
    {var, Name}.

unwrap({_,_,V}) -> V.
