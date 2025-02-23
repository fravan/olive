//// The `watcher` module watches a list of dir and triggers a `FilesChanged` message
//// to be handled by calling code.

import filepath
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject, type Timer}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type ErlangStartResult}
import gleam/result
import gleam/string
import olive/config
import olive/logging
import olive/utils
import simplifile

type WatcherError {
  NoFileWatcherSupportedForOs
  NoFileWatcherInstalled(watcher: dynamic.Dynamic)
}

@external(erlang, "fs", "start_link")
fn fs_start_link(name: Atom, path: String) -> ErlangStartResult

@external(erlang, "fs", "subscribe")
fn fs_subscribe(name: Atom) -> Atom

@external(erlang, "olive_ffi", "get_cwd")
fn get_cwd() -> Result(String, Atom)

@external(erlang, "olive_ffi", "check_watcher_installed")
fn check_watcher_installed() -> Result(Nil, WatcherError)

pub type Message {
  FilesChanged
}

type State {
  State(debounce_timer: Option(Timer), watch_subject: Subject(Message))
}

pub fn start(config: config.Config, watch_subject: Subject(Message)) {
  actor.start_spec(
    actor.Spec(init_timeout: 5000, loop: do_loop, init: fn() {
      init_watcher(config, watch_subject)
    }),
  )
}

fn init_watcher(config: config.Config, watch_subject: Subject(Message)) {
  check_watcher_install()
  |> result.then(check_directories(config, _))
  |> result.map(start_watchers)
  |> result.map(fn(selectors) {
    actor.Ready(State(None, watch_subject), selectors)
  })
  |> result.map_error(fn(err) { actor.Failed(err) })
  |> result.unwrap_both
}

fn check_watcher_install() {
  case check_watcher_installed() {
    Error(NoFileWatcherSupportedForOs) ->
      Error("No file watcher supported for your OS")
    Error(NoFileWatcherInstalled(watcher)) ->
      Error(
        "No file watcher installed, please install: " <> string.inspect(watcher),
      )
    Ok(_) -> Ok(Nil)
  }
}

fn check_directories(config: config.Config, _) -> Result(List(String), String) {
  get_cwd()
  |> result.map(io.debug)
  |> result.map_error(fn(posix) {
    "Could not get current working dir, error is: " <> atom.to_string(posix)
  })
  |> result.then(get_root)
  |> result.then(fn(root) {
    let dirs = case config.dirs {
      [] -> ["src"]
      _ -> config.dirs
    }

    let #(oks, errors) =
      dirs
      |> list.map(fn(dir) {
        let full_path = filepath.join(root, dir)
        filepath.expand(full_path)
        |> result.map_error(fn(_) { "Could not find dir at " <> full_path })
        |> result.try(fn(dir) {
          case simplifile.is_directory(dir) {
            Ok(True) -> Ok(dir)
            Ok(False) | Error(_) ->
              Error("Path: \"" <> dir <> "\" is not a directory")
          }
        })
      })
      |> result.partition

    case errors {
      [] -> Ok(oks)
      _ -> Error(string.join(errors, with: "\n"))
    }
  })
}

fn start_watchers(dirs: List(String)) {
  dirs
  |> list.map(watch_folder)
  |> list.fold(from: process.new_selector(), with: process.merge_selector)
}

fn get_root(path: String) {
  case simplifile.is_file(filepath.join(path, "gleam.toml")) {
    Ok(True) -> Ok(path)
    Ok(False) | Error(_) -> {
      case filepath.expand(filepath.join(path, "..")) {
        Ok(path) -> get_root(path)
        Error(_) -> Error("Could not locate root dir where gleam.toml is")
      }
    }
  }
}

fn do_loop(msg: InternalMsg, state: State) {
  case msg {
    IgnoreChanges -> actor.continue(state)
    TriggerFilesChanged -> {
      maybe_cancel_timer(state.debounce_timer)
      // Watcher sends multiple events for a same save,
      // so we debounce it to avoid multiple builds in a very short time
      let timer = process.send_after(state.watch_subject, 50, FilesChanged)
      actor.continue(State(..state, debounce_timer: Some(timer)))
    }
  }
}

pub opaque type InternalMsg {
  TriggerFilesChanged
  IgnoreChanges
}

fn maybe_cancel_timer(timer: Option(Timer)) {
  use timer <- utils.from_option(timer)
  process.cancel_timer(timer)
  Nil
}

fn watch_folder(dir: String) {
  let atom = atom.create_from_string("fs_watcher_" <> dir)
  // the supervisor started by the fs lib always return {ok}
  let assert Ok(_) = fs_start_link(atom, dir)
  fs_subscribe(atom)
  let selectors =
    process.new_selector()
    |> process.selecting_anything(watch_decoder)
  selectors
}

fn watch_decoder(msg: decode.Dynamic) {
  let decoder = {
    use events <- decode.subfield([2, 1], decode.list(atom_to_watch_events()))
    decode.success(events)
  }
  case decode.run(msg, decoder) {
    Ok(events) ->
      case list.contains(events, EventNeedingRebuild) {
        True -> TriggerFilesChanged
        False -> IgnoreChanges
      }
    Error(_) -> {
      logging.error("Error occured while watching files")
      IgnoreChanges
    }
  }
}

type WatchEvents {
  EventNeedingRebuild
  OtherEvents
}

/// Converts an atom to an event
fn atom_to_watch_events() {
  // Only modified and renamed files are interesting
  // Reasoning:
  // - Modified code needs a reload, it's trivial
  // - Renamed file could be automated with LSP, modifying other files referencing them.
  //   we might ignore it as we would have other Modified event for other files.
  //   but just in case, let's listen for that.
  // - Created / Deleted file are either empty or not used anymore, we can spare
  //   the build (would either error or produce nothing new)
  // - Other events don't need a rebuild?
  let modified = atom.create_from_string("modified")
  let renamed = atom.create_from_string("renamed")

  decode.new_primitive_decoder("Atom", fn(data) {
    case atom.from_dynamic(data) {
      Ok(ev) if ev == modified -> Ok(EventNeedingRebuild)
      Ok(ev) if ev == renamed -> Ok(EventNeedingRebuild)
      _ -> Ok(OtherEvents)
    }
  })
}
