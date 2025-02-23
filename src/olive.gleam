import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import olive/cli
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
  case cli.get_options() {
    Error(Nil) -> Nil
    Ok(options) -> {
      logging.configure_logs(options.log)

      case config.read_config(options) {
        Error(error) -> logging.error(error)
        Ok(config) -> run_olive(config)
      }

      // logging is async, so we wait just a skosh…
      process.sleep(400)
    }
  }
}

fn run_olive(config: config.Config) {
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
      case server_run.start_server(config.root, config.name) {
        Ok(_pid) -> {
          let assert Ok(_) = proxy.start_http(config, clients)

          listen_to_file_changes(config, subject, clients)
        }
        Error(reason) -> {
          logging.error(
            "Could not start server for this reason: " <> atom.to_string(reason),
          )
        }
      }
    }
  }
}

fn listen_to_file_changes(
  config: config.Config,
  subject: Subject(watcher.Message),
  clients: ClientRegistry,
) {
  let msg = process.receive_forever(subject)
  case msg {
    watcher.FilesChanged(file_name) -> {
      logging.notice("File changed: " <> file_name)
      case server_run.reload_server_code(config.root) {
        Ok(_) -> {
          client_registry.trigger(clients)
        }
        Error(msg) -> {
          logging.error(msg)
        }
      }
    }
  }
  listen_to_file_changes(config, subject, clients)
}
