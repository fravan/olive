//// the `server_run` module runs the "main" server and also handles the rebuilding
//// of the main server code every time it is needed.

import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/result
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

pub fn start_server(root: String, name: String) {
  let assert Ok(fully_qualified_module) = atom.from_string(name <> "@@main")
  let assert Ok(module) = atom.from_string(name)
  spawn_main_server(root, fully_qualified_module, module)
}

pub fn reload_server_code(root: String) {
  shellout.command(run: "gleam", with: ["build"], in: root, opt: [])
  |> result.map_error(fn(err) {
    let #(status, msg) = err
    "Error while building gleam project:\n"
    <> int.to_string(status)
    <> " - "
    <> msg
  })
  |> result.try(fn(output) {
    logging.notice("Output of `gleam build`:\n" <> output)
    reload_modules()
    |> result.map_error(fn(_) { "Error while reloading Erlang modules" })
  })
}
