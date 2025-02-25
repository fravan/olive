//// the `server_run` module runs the "main" server and also handles the rebuilding
//// of the main server code every time it is needed.

import gleam/erlang/atom
import gleam/erlang/process
import gleam/result
import olive/config
import olive/logging
import shellout

// From gleam-radiate
type Module

type What

@external(erlang, "olive_ffi", "reload_modules")
fn reload_modules() -> Result(Nil, List(#(Module, What)))

@external(erlang, "olive_ffi", "spawn_main_server")
fn spawn_main_server(
  root: String,
  fully_qualified_module: atom.Atom,
  module: atom.Atom,
) -> Result(process.Pid, atom.Atom)

pub fn start_server(config: config.Config) {
  use _ <- result.try(build_server(config))
  use fully_qualified_module <- result.try(get_atom(config.name <> "@@main"))
  use module <- result.try(get_atom(config.name))
  spawn_main_server(config.root, fully_qualified_module, module)
  |> result.map_error(posix_error_to_string(config, _))
}

pub fn reload_server_code(config: config.Config) {
  build_server(config)
  |> result.try(fn(_output) {
    reload_modules()
    |> result.map_error(fn(_) { "Error while reloading Erlang modules" })
  })
}

fn build_server(config: config.Config) {
  logging.notice(config.logger, "Launching new build via `gleam build`")
  shellout.command(run: "gleam", with: ["build"], in: config.root, opt: [
    shellout.LetBeStderr,
    shellout.LetBeStdout,
  ])
  |> result.replace_error(
    "Error while building gleam project, see gleam output ☝️",
  )
}

fn get_atom(name: String) {
  atom.from_string(name)
  |> result.replace_error("Cannot find loaded atom " <> name)
}

fn posix_error_to_string(config: config.Config, posix: atom.Atom) {
  "Cannot launch server in "
  <> config.root
  <> " because of the following: "
  <> atom.to_string(posix)
}
