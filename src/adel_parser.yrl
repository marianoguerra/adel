Nonterminals

doc tags tag attrs attr value attr_block body_block.

Terminals

tagprefix identifier open close open_block close_block string integer float bool white sep dot colon.

Rootsymbol doc.

doc -> tags: '$1'.

tags -> tag : ['$1'].
tags -> tag tags : ['$1'|'$2'].

tag -> tagprefix identifier : tag(unwrap('$2')).
tag -> tagprefix identifier attr_block: tag(unwrap('$2'), '$3').
tag -> tagprefix identifier body_block: tag(unwrap('$2'), [], '$3').
tag -> tagprefix identifier attr_block body_block: tag(unwrap('$2'), '$3', '$4').

tag -> tagprefix attr_block: tag('', '$2').
tag -> tagprefix body_block: tag('', [], '$2').
tag -> tagprefix attr_block body_block: tag('', '$2', '$3').

tag -> white: tag('!white', [], unwrap('$1')).
tag -> string: tag('!string', [], unwrap('$1')).

attr_block -> open close: [].
attr_block -> open attrs close: '$2'.

body_block -> open_block close_block: [].
body_block -> open_block tags close_block: '$2'.

attrs -> attr : ['$1'].
attrs -> attr sep attrs : ['$1'|'$3'].

attr -> identifier colon value: attr(unwrap('$1'), unwrap('$3')).

value -> integer: '$1'.
value -> float:   '$1'.
value -> string:  '$1'.
value -> bool:    '$1'.

Erlang code.

tag(Name) ->
    tag(Name, [], []).

tag(Name, Attrs) ->
    tag(Name, Attrs, []).

tag(Name, Attrs, Childs) ->
    {tag, Name, Attrs, Childs}.

attr(Name, Val) ->
    {attr, Name, Val}.

unwrap({_,_,V}) -> V.
