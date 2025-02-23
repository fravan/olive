-module(olive_ffi).

-export([reload_modules/0, spawn_main_server/2, configure_logs/1, log/2, format/2]).

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
