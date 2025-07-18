//// the `server_run` module runs the "main" server and also handles the rebuilding
//// of the main server code every time it is needed.

import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/result
import gleam/string
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
  use _ <- result.try(build_server(config, config.dirs))
  use fully_qualified_module <- result.try(get_atom(config.name <> "@@main"))
  use module <- result.try(get_atom(config.name))
  spawn_main_server(config.root, fully_qualified_module, module)
  |> result.map_error(posix_error_to_string(config, _))
}

pub fn reload_server_code(
  config: config.Config,
  changed_dirs: List(config.Directory),
) {
  build_server(config, changed_dirs)
  |> result.try(fn(_output) {
    reload_modules()
    |> result.map_error(fn(_) { "Error while reloading Erlang modules" })
  })
}

fn build_server(config: config.Config, changed_dirs: List(config.Directory)) {
  changed_dirs
  |> list.filter_map(fn(changed_dir) {
    case changed_dir {
      config.SourceDirectory(path:, uses_lustre_dev_tools: True) -> Ok(path)
      _ -> Error(Nil)
    }
  })
  |> list.each(fn(changed_src_path) {
    let result = {
      use root <- result.try(config.get_root(changed_src_path))
      run_build(config, with: ["run", "-m", "lustre/dev", "build"], in: root)
    }

    case result {
      Ok(_) -> Nil
      Error(msg) -> logging.error(config.logger, msg)
    }
  })

  run_build(config, with: ["build"], in: config.root)
}

fn run_build(
  config: config.Config,
  with arguments: List(String),
  in directory: String,
) {
  let executable = "gleam"

  logging.notice(
    config.logger,
    "Launching new build via `"
      <> executable
      <> " "
      <> string.join(arguments, " ")
      <> "` in `"
      <> directory
      <> "`",
  )

  shellout.command(run: executable, with: arguments, in: directory, opt: [
    shellout.LetBeStderr,
    shellout.LetBeStdout,
  ])
  |> result.replace_error(
    "Error while building gleam project, see gleam output ☝️",
  )
}

fn get_atom(name: String) {
  atom.get(name)
  |> result.replace_error("Cannot find loaded atom " <> name)
}

fn posix_error_to_string(config: config.Config, posix: atom.Atom) {
  "Cannot launch server in "
  <> config.root
  <> " because of the following: "
  <> atom.to_string(posix)
}
