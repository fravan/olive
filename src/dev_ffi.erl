-module(dev_ffi).

-export([atomic_load/1, spawn_main_server/2]).

atomic_load(Modules) ->
    case code:atomic_load(Modules) of
        ok ->
            {ok, nil};
        otherwise ->
            otherwise
    end.

spawn_main_server(FullyQualifiedModule, Module) ->
    spawn_link(FullyQualifiedModule, run, [Module]).
