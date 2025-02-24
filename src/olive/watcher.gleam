//// The `watcher` module watches a list of dir and triggers a `FilesChanged` message
//// to be handled by calling code.

import filepath
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist
import gleam/erlang/process.{type Subject, type Timer}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type ErlangStartResult}
import gleam/string
import olive/config
import olive/logging

type WatcherError {
  NoFileWatcherSupportedForOs
  NoFileWatcherInstalled(watcher: dynamic.Dynamic)
}

@external(erlang, "fs", "start_link")
fn fs_start_link(name: Atom, path: String) -> ErlangStartResult

@external(erlang, "fs", "subscribe")
fn fs_subscribe(name: Atom) -> Atom

@external(erlang, "olive_ffi", "check_watcher_installed")
fn check_watcher_installed() -> Result(Nil, WatcherError)

pub type Message {
  FilesChanged(String)
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
  case check_watcher_install() {
    Error(err) -> actor.Failed(err)
    Ok(_) -> {
      let selectors = start_watchers(config)
      actor.Ready(State(None, watch_subject), selectors)
    }
  }
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

fn start_watchers(config: config.Config) {
  config.dirs
  |> list.map(watch_folder(config.logger, _))
  |> list.fold(from: process.new_selector(), with: process.merge_selector)
}

fn do_loop(msg: InternalMsg, state: State) {
  case msg {
    IgnoreChanges -> actor.continue(state)
    TriggerFilesChanged(file_name) -> {
      maybe_cancel_timer(state.debounce_timer)
      // Watcher sends multiple events for a same save,
      // so we debounce it to avoid multiple builds in a very short time
      let timer =
        process.send_after(state.watch_subject, 50, FilesChanged(file_name))
      actor.continue(State(..state, debounce_timer: Some(timer)))
    }
  }
}

pub opaque type InternalMsg {
  TriggerFilesChanged(String)
  IgnoreChanges
}

fn maybe_cancel_timer(timer: Option(Timer)) {
  case timer {
    option.None -> Nil
    option.Some(timer) -> {
      process.cancel_timer(timer)
      Nil
    }
  }
}

fn watch_folder(logger: logging.Logger, dir: String) {
  let atom = atom.create_from_string("fs_watcher_" <> dir)
  // the supervisor started by the fs lib always return {ok}
  let assert Ok(_) = fs_start_link(atom, dir)
  fs_subscribe(atom)
  let selectors =
    process.new_selector()
    |> process.selecting_anything(watch_decoder(logger, _))
  selectors
}

fn watch_decoder(logger: logging.Logger, msg: decode.Dynamic) {
  let decoder = {
    use file_name <- decode.subfield([2, 0], erlang_string_to_string_decoder())
    use events <- decode.subfield([2, 1], decode.list(atom_to_watch_events()))

    decode.success(#(file_name, events))
  }
  case decode.run(msg, decoder) {
    Ok(#(file_name, events)) ->
      case
        filepath.extension(file_name),
        list.contains(events, EventNeedingRebuild)
      {
        Ok("gleam"), True -> TriggerFilesChanged(file_name)
        _, _ -> IgnoreChanges
      }
    Error(decode_errors) -> {
      logging.error(logger, "Error occured while watching files")
      IgnoreChanges
    }
  }
}

type WatchEvents {
  EventNeedingRebuild
  OtherEvents
}

@external(erlang, "olive_ffi", "coerce")
fn coerce(item: a) -> b

fn erlang_string_to_string_decoder() {
  decode.new_primitive_decoder("ErlangString", fn(data) {
    coerce(data) |> charlist.to_string |> Ok
  })
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
