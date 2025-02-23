import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/option
import gleam/otp/actor
import olive/client_registry.{type ClientRegistry}
import olive/config
import olive/logging
import olive/proxy
import olive/server_run
import olive/utils
import olive/watcher

/// Process is as follow:
/// - Start a client registry
/// - Start watcher, giving it a subject we will listen to.
/// - Every time watcher sends us a message:
///   - Reload server code
///   - Send reload message to clients
pub fn main() {
  // TODO: read config CLI before parsing gleam.toml
  logging.configure_logs(logging.AllLogs)
  case config.parse_from_cli() {
    option.Some(config) -> run_olive(config)
    option.None -> Nil
  }
  process.sleep_forever()
}

fn run_olive(config: config.Config) {
  logging.configure_logs(config.log)

  let clients = client_registry.start()
  let subject = process.new_subject()

  case watcher.start(config, subject) {
    Error(actor.InitTimeout) ->
      logging.error("Watcher took too much time to initialize")
    Error(actor.InitCrashed(_)) -> logging.error("Watcher crashed!")
    Error(actor.InitFailed(process.Abnormal(msg))) -> logging.error(msg)
    Error(actor.InitFailed(process.Killed)) ->
      logging.error("Watcher was killed before initialisation")
    Error(actor.InitFailed(process.Normal)) ->
      logging.error("Watcher shutdown before end of initialisation")
    Ok(_) -> {
      logging.notice("Watcher could start, goodbye!")
      // let _ = server_run.start_server()

      // let assert Ok(_) = proxy.start_http(config, clients)

      // listen_to_file_changes(subject, clients)
    }
  }
}

fn listen_to_file_changes(
  subject: Subject(watcher.Message),
  clients: ClientRegistry,
) {
  let msg = process.receive_forever(subject)
  case msg {
    watcher.FilesChanged -> {
      case server_run.reload_server_code() {
        Ok(_) -> {
          client_registry.trigger(clients)
        }
        Error(msg) -> {
          logging.error(msg)
        }
      }
    }
  }
  listen_to_file_changes(subject, clients)
}
