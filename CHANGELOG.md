# Changelog

## Unrealeased

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
