//// The `watcher` module watches a list of dir and triggers a `FilesChanged` message
//// to be handled by calling code.

import dev_server/logging
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject, type Timer}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type ErlangStartResult}

@external(erlang, "fs", "start_link")
fn fs_start_link(name: Atom, path: String) -> ErlangStartResult

@external(erlang, "fs", "subscribe")
fn fs_subscribe(name: Atom) -> Atom

pub type Message {
  FilesChanged
}

type State {
  State(debounce_timer: Option(Timer), watch_subject: Subject(Message))
}

pub fn start(watch_subject: Subject(Message)) {
  actor.start_spec(
    actor.Spec(init_timeout: 5000, loop: do_loop, init: fn() {
      case watch_folder("src") {
        Ok(selectors) -> actor.Ready(State(None, watch_subject), selectors)
        Error(_) -> actor.Failed("Could not start watch actor with folder src")
      }
    }),
  )
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
  case timer {
    Some(timer) -> {
      process.cancel_timer(timer)
      Nil
    }
    None -> Nil
  }
}

fn watch_folder(dir: String) {
  let atom = atom.create_from_string("fs_watcher_" <> dir)
  case fs_start_link(atom, dir) {
    Ok(_pid) -> {
      fs_subscribe(atom)
      let selectors =
        process.new_selector()
        |> process.selecting_anything(watch_decoder)
      Ok(selectors)
    }
    Error(_) -> {
      logging.log_error("Error occured while watching folder: " <> dir)
      Error(Nil)
    }
  }
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
      logging.log_error("Error occured while watching files")
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
