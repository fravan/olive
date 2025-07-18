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
import gleam/otp/actor
import gleam/set.{type Set}
import gleam/string
import olive/config
import olive/logging

type WatcherError {
  NoFileWatcherSupportedForOs
  NoFileWatcherInstalled(watcher: dynamic.Dynamic)
}

@external(erlang, "fs", "start_link")
fn fs_start_link(name: Atom, path: String) -> Result(Nil, Nil)

@external(erlang, "fs", "subscribe")
fn fs_subscribe(name: Atom) -> Atom

@external(erlang, "olive_ffi", "check_watcher_installed")
fn check_watcher_installed() -> Result(Nil, WatcherError)

pub type Change {
  SourceChange(file_name: String)
  PrivChange(file_name: String)
}

pub type Message {
  FilesChanged(List(Change))
}

type State {
  State(
    debounce_timer: Option(Timer),
    debounce_in_ms: Int,
    watch_subject: Subject(Message),
    actor_subject: Subject(InternalMsg),
    current_changes: Set(Change),
  )
}

pub fn start(config: config.Config, watch_subject: Subject(Message)) {
  actor.new_with_initialiser(5000, fn(_messages) {
    case check_watcher_install() {
      Error(err) -> Error(err)
      Ok(_) -> {
        let subject = process.new_subject()
        start_directory_watchers(config, subject)
        let selector =
          process.new_selector()
          |> process.select(subject)
        Ok(
          actor.initialised(State(
            None,
            config.debounce_in_ms,
            watch_subject,
            subject,
            set.new(),
          ))
          |> actor.selecting(selector),
        )
      }
    }
  })
  |> actor.on_message(do_loop)
  |> actor.start
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

fn start_directory_watchers(
  config: config.Config,
  subject: process.Subject(InternalMsg),
) {
  config.dirs
  |> list.each(start_directory_watcher(config.logger, subject, _))
}

pub opaque type InternalMsg {
  TriggerFilesChanged(file_name: String, dir: config.Directory)
  TriggerWatcher
  IgnoreChanges
}

fn do_loop(state: State, msg: InternalMsg) {
  case msg {
    IgnoreChanges -> actor.continue(state)
    TriggerWatcher -> {
      process.send(
        state.watch_subject,
        FilesChanged(set.to_list(state.current_changes)),
      )
      actor.continue(
        State(..state, debounce_timer: None, current_changes: set.new()),
      )
    }
    TriggerFilesChanged(file_name, dir) -> {
      maybe_cancel_timer(state.debounce_timer)
      // Watcher sends multiple events for a same save,
      // so we debounce it to avoid multiple builds in a very short time
      // we also avoid adding it again to the current debounced changes
      let new_change = case dir {
        config.SourceDirectory(_) -> SourceChange(file_name)
        config.PrivDirectory(_) -> PrivChange(file_name)
      }
      let current_changes = set.insert(state.current_changes, new_change)
      let timer =
        process.send_after(
          state.actor_subject,
          state.debounce_in_ms,
          TriggerWatcher,
        )
      actor.continue(
        State(..state, debounce_timer: Some(timer), current_changes:),
      )
    }
  }
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

fn start_directory_watcher(
  logger: logging.Logger,
  subject: process.Subject(InternalMsg),
  dir: config.Directory,
) {
  // Each watcher lives it its own process so it can listen to the messages
  // sent by the `fs` library.
  process.spawn(fn() {
    let atom = atom.create("fs_watcher_" <> dir.path)
    // the supervisor started by the fs lib always return {ok}
    let assert Ok(_) = fs_start_link(atom, dir.path)
    fs_subscribe(atom)
    let selector =
      process.new_selector()
      |> process.select_other(watch_decoder(logger, dir, _))

    listen_directory_watcher(selector, subject)
  })
}

fn listen_directory_watcher(
  selector: process.Selector(InternalMsg),
  subject: process.Subject(InternalMsg),
) {
  let msg = process.selector_receive_forever(selector)
  process.send(subject, msg)
  listen_directory_watcher(selector, subject)
}

fn watch_decoder(
  logger: logging.Logger,
  dir: config.Directory,
  msg: decode.Dynamic,
) {
  let decoder = {
    use file_name <- decode.subfield([2, 0], erlang_string_to_string_decoder())
    use events <- decode.subfield([2, 1], decode.list(atom_to_watch_events()))

    decode.success(#(file_name, events))
  }
  case decode.run(msg, decoder), dir {
    Ok(#(file_name, events)), config.SourceDirectory(_) ->
      case filepath.extension(file_name), list.contains(events, UpdatedFile) {
        Ok("gleam"), True -> TriggerFilesChanged(file_name, dir)
        _, _ -> IgnoreChanges
      }
    Ok(#(file_name, events)), config.PrivDirectory(_) -> {
      case list.contains(events, UpdatedFile) {
        True -> TriggerFilesChanged(file_name, dir)
        False -> IgnoreChanges
      }
    }
    Error(decode_errors), _ -> {
      let msg =
        list.map(decode_errors, fn(error) {
          let decode.DecodeError(expected, found, path) = error
          "Expected "
          <> expected
          <> " at ["
          <> string.join(path, ",")
          <> "] but found "
          <> found
        })
      logging.error(
        logger,
        "Error occured while watching files:\n" <> string.join(msg, "\n"),
      )
      IgnoreChanges
    }
  }
}

type WatchEvents {
  UpdatedFile
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
  let modified = atom.create("modified")
  let renamed = atom.create("renamed")

  decode.new_primitive_decoder("Atom", fn(data) {
    case atom.cast_from_dynamic(data) {
      ev if ev == modified -> Ok(UpdatedFile)
      ev if ev == renamed -> Ok(UpdatedFile)
      _ -> Ok(OtherEvents)
    }
  })
}
