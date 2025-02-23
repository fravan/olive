-module(olive_ffi).

-export([get_cwd/0, reload_modules/0, spawn_main_server/2, configure_logs/1, log/2,
         format/2, check_watcher_installed/0]).

get_cwd() ->
    case file:get_cwd() of
        {ok, Dir} ->
            {ok, list_to_binary(Dir)};
        {error, Reason} ->
            {error, Reason}
    end.

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

configure_logs(Level) ->
    logger:add_handler(olive_log,
                       logger_std_h,
                       #{formatter => {olive_ffi, #{}},
                         filters =>
                             [{olive_filter_domain,
                               {fun logger_filters:domain/2, {stop, not_equal, [olive]}}},
                              {olive_filter_level,
                               {fun logger_filters:level/2, {stop, lt, Level}}}],
                         config => #{type => standard_io}}).

log(Level, Message) ->
    logger:log(Level, Message, #{domain => [olive]}).

format(#{level := Level,
         msg := Msg,
         meta := _Meta},
       _Config) ->
    [format_level(Level), format_msg(Msg), $\n].

format_level(Level) ->
    case Level of
        emergency ->
            "\x1b[1;41mOLIVE - EMRG\x1b[0m";
        alert ->
            "\x1b[1;41mOLIVE - ALRT\x1b[0m";
        critical ->
            "\x1b[1;41mOLIVE - CRIT\x1b[0m";
        error ->
            "\x1b[1;31mOLIVE - ERROR\x1b[0m";
        warning ->
            "\x1b[1;33mOLIVE - WARN\x1b[0m";
        notice ->
            "\x1b[1;32mOLIVE - NTCE\x1b[0m";
        info ->
            "\x1b[1;34mOLIVE - INFO\x1b[0m";
        debug ->
            "\x1b[1;36mOLIVE - DEBG\x1b[0m"
    end.

format_msg(Report0) ->
    case Report0 of
        {string, Msg} ->
            [$\s, Msg];
        _ ->
            [$\s, gleam@string:inspect(Report0)]
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
