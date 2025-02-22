import gleam/erlang/process.{type Subject}
import gleam/option
import olive/client_registry.{type ClientRegistry}
import olive/config
import olive/logging
import olive/proxy
import olive/server_run
import olive/watcher

/// Process is as follow:
/// - Start a client registry
/// - Start watcher, giving it a subject we will listen to.
/// - Every time watcher sends us a message:
///   - Reload server code
///   - Send reload message to clients
pub fn main() {
  let config = config.parse_from_cli()
  case config {
    option.Some(config) -> run_olive(config)
    option.None -> {
      Nil
    }
  }
}

fn run_olive(config: config.Config) {
  logging.configure_logs(config.log)

  let clients = client_registry.start()
  let subject = process.new_subject()
  let assert Ok(_) = watcher.start(subject)
  let _ = server_run.start_server()

  let assert Ok(_) = proxy.start_http(config, clients)

  listen_to_file_changes(subject, clients)
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
