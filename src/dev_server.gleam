import dev_server/client_registry.{type ClientRegistry}
import dev_server/logging
import dev_server/proxy
import dev_server/server_run
import dev_server/watcher
import gleam/erlang/process.{type Subject}

/// Process is as follow:
/// - Start a client registry
/// - Start watcher, giving it a subject we will listen to.
/// - Every time watcher sends us a message:
///   - Reload server code
///   - Send reload message to clients
pub fn main() {
  let clients = client_registry.start()
  let subject = process.new_subject()
  let assert Ok(_) = watcher.start(subject)
  let _ = server_run.start_server()

  let assert Ok(_) = proxy.start_http(clients)

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
          logging.log_error("Error while reloading server code : " <> msg)
        }
      }
    }
  }
  listen_to_file_changes(subject, clients)
}
