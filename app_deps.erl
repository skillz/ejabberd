%%% Run with 'escript app_deps.erl'
%%% Change the path in filelib:wildcard/1 as required to capture all
%%% your dependencies.
%%%
%%% Rectangular nodes will represent library apps (no processes involved)
%%% and the circular nodes will represent regular apps. An arrow going from
%%% 'A -> B' means 'A depends on B'.
%%%
%%% This script depends on graphviz being present on the system.
-module(app_deps).
-export([main/1]).

main(_) ->
    AppFiles = filelib:wildcard("deps/*/ebin/*.app")
               ++
               filelib:wildcard("ebin/*.app"),
    Deps = [{App, proplists:get_value(applications, Props, []), apptype(Props)}
            || {ok, [{_,App,Props}]} <-
               [file:consult(AppFile) || AppFile <- AppFiles]],
    to_graphviz(Deps).

apptype(Props) ->
    case proplists:get_value(mod, Props) of
        undefined -> library;
        _ -> regular
    end.

to_graphviz(Deps) ->
    Bytes = ["digraph G { ",
             "K=0.25; ",
             "ratio=0.75; ",
             "overlap=\"9:prism\"; ",
             [io_lib:format("~p [shape=box] ", [App])
              || {App, _, library} <- Deps],
             [[io_lib:format("~p->~p ", [App,Dep]) || Dep <- DepList -- [kernel, stdlib]]
              || {App, DepList, _} <- Deps],
             "}"],
    file:write_file("app-deps.dot", Bytes),
io:format("~s",[os:cmd("dot app-deps.dot -Tpng -o app-deps.png")]).
