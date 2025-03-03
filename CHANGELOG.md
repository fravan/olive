# Changelog

## v1.4.0 - 2025-03-XX

### New features
- Olive can now watch for `priv` directory, and any changes in them. It will trigger clients to reload __WITHOUT__ rebuilding your project.
- Added `watch_debounce` option in CLI.

## v1.3.1 - 2025-03-03
- Relaxed `gleam_http` constraint to permit v4.

## v1.3.0 - 2025-02-25

### New features
- Stop olive when the main file is updated and log a warning message to the user, prompting them to restart olive.

### Fixes
- Let `gleam build` show its original output instead of catching it (this way, we keep the colours, as well as the output even with olive logs disabled)
- Force a `gleam build` when starting server for the first time

### Minor changes
- Updated some formats on logger output

## v1.2.0 - 2025-02-23

### Project dependencies 🛠
Olive will parse your `gleam.toml` file to search for any dependencies you have with a `{ path = ".." }` resolution
and will watch those projects as well (under src all the time).

Once initialised, Olive will tell you the list of folders currently on watch.

> _Oh, so sorry, I forgot about the autowatch_
>
> Korben, 1997.

### Minor changes
- Olive will tell you what file triggered a reload
- Olive only watches for `.gleam` files to avoid some crazy looping behaviours
- Olive can be run from any folder and will go up the tree until it reaches a `gleam.toml`

## v1.1.0 - 2025-02-22

- Added `main_port` option
- Added `proxy_port` option
- Added `bind` option
- Added `log` option

All of the above options can be seen with `gleam run -m olive -- --help`

## v1.0.0 - 2025-02-21

Initial release for Olive
