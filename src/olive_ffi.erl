-module(olive_ffi).

-export([reload_modules/0, spawn_main_server/2]).

spawn_main_server(FullyQualifiedModule, Module) ->
    spawn_link(FullyQualifiedModule, run, [Module]).

reload_modules() ->
    Modules = code:modified_modules(),
    lists:foreach(fun(Mod) -> code:purge(Mod) end, Modules),
    atomic_load(Modules).

atomic_load(Modules) ->
    case code:atomic_load(Modules) of
        ok ->
            {ok, nil};
        otherwise ->
            otherwise
    end.
