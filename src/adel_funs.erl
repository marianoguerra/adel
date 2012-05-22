-module(adel_funs).
-export([eval/1, apply/2]).

apply(append, {cons, Root, Args}) ->
    append(Root, Args);

apply(Name, Args) ->
    throw({unknown_function, Name, Args}).

append(Root, nil) ->
    Root;

append({tag, Name, Attrs, Body}, {cons, H, T}) ->
    Result = eval(H),
    append({tag, Name, Attrs, Body ++ [Result]}, T).

eval({cons, {identifier, _, FunName}, Args}) ->
    adel_funs:apply(FunName, Args);

eval(Other) ->
    Other.
