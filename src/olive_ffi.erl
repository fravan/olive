-module(olive_ffi).

-export([get_cwd/0, reload_modules/0, spawn_main_server/3, check_watcher_installed/0,
         coerce/1]).

coerce(X) ->
    X.

get_cwd() ->
    case file:get_cwd() of
        {ok, Dir} ->
            {ok, list_to_binary(Dir)};
        {error, Reason} ->
            {error, Reason}
    end.

spawn_main_server(Root, FullyQualifiedModule, Module) ->
    % We want to make sure we execute the main server in the ROOT dir.
    case file:set_cwd(Root) of
        ok ->
            {ok, spawn_link(FullyQualifiedModule, run, [Module])};
        {error, Reason} ->
            {error, Reason}
    end.

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

% From: https://github.com/lustre-labs/dev-tools
% This is what the underlying `fs` library does to check if it has support for
% a given os:
%
% https://github.com/5HT/fs/blob/23a5b46b033437a3d69504811ae6c72f7704a78a/src/fs_sup.erl#L18-L46
%
% Sadly the library doesn't expose such a function and just logs any error
% instead of surfacing it as a value, so we have to implement a slightly
% modified version of it to have proper error messages.
check_watcher_installed() ->
    Watcher =
        case os:type() of
            {unix, darwin} ->
                fsevents;
            {unix, linux} ->
                inotifywait;
            {unix, sunos} ->
                undefined;
            {unix, _} ->
                kqueue;
            {win32, nt} ->
                inotifywait_win32;
            _ ->
                undefined
        end,

    case Watcher of
        undefined ->
            {error, no_file_watcher_supported_for_os};
        _ ->
            case Watcher:find_executable() of
                false ->
                    {error, {no_file_watcher_installed, Watcher}};
                _ ->
                    {ok, nil}
            end
    end.
