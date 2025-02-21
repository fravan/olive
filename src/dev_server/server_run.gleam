//// the `server_run` module runs the "main" server and also handles the rebuilding
//// of the main server code every time it is needed. Once a new build is generated
//// it _hot_ replaces the updated modules.

import dev_server/config
import dev_server/logging
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result
import shellout

// From gleam-radiate
type Module

type What

@external(erlang, "code", "modified_modules")
fn modified_modules() -> List(Module)

@external(erlang, "dev_ffi", "spawn_main_server")
fn spawn_main_server(
  fully_qualified_module: atom.Atom,
  module: atom.Atom,
) -> process.Pid

@external(erlang, "code", "purge")
fn purge(module: Module) -> Bool

@external(erlang, "dev_ffi", "atomic_load")
fn atomic_load(modules: List(Module)) -> Result(Nil, List(#(Module, What)))

pub fn start_server() {
  let name = config.get_name()
  let assert Ok(fully_qualified_module) = atom.from_string(name <> "@@main")
  let assert Ok(module) = atom.from_string(name)
  spawn_main_server(fully_qualified_module, module)
}

pub fn reload_server_code() {
  shellout.command(run: "gleam", with: ["build"], in: ".", opt: [])
  |> result.map_error(fn(err) {
    let #(status, msg) = err
    "Error while building gleam project: "
    <> int.to_string(status)
    <> " - "
    <> msg
  })
  |> result.try(fn(output) {
    logging.log_debug("Output of `gleam build`: " <> output)
    let mods = modified_modules()
    list.each(mods, purge)
    atomic_load(mods)
    |> result.map_error(fn(_) { "Error while reloading Erlang modules" })
  })
}
