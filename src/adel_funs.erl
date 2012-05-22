-module(adel_funs).
-export([apply/2]).

apply(append, {cons, Root, Args}) ->
    append(Root, Args);

apply(Name, Args) ->
    throw({unknown_function, Name, Args}).

append(Root, nil) ->
    Root;

append({tag, Name, Attrs, Body}, {cons, H, T}) ->
    append({tag, Name, Attrs, Body ++ [H]}, T).
