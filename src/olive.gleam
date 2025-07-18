import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor
import gleam/string
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
      let logger = logging.get_logger(options.log)

      case config.read_config(logger, options) {
        Error(error) -> logging.error(logger, error)
        Ok(config) -> run_olive(config)
      }
    }
  }
}

fn run_olive(config: config.Config) {
  let clients = client_registry.start(config.logger)
  let subject = process.new_subject()

  case watcher.start(config, subject) {
    Error(actor.InitTimeout) ->
      logging.error(config.logger, "Watcher took too much time to initialize")
    Error(actor.InitFailed(msg)) -> logging.error(config.logger, msg)
    Error(actor.InitExited(process.Killed)) ->
      logging.error(config.logger, "Watcher was killed before initialisation")
    Error(actor.InitExited(process.Abnormal(_))) ->
      logging.error(config.logger, "Watcher shutdown abnormally")
    Error(actor.InitExited(process.Normal)) ->
      logging.error(
        config.logger,
        "Watcher shutdown before end of initialisation",
      )
    Ok(_) -> {
      case server_run.start_server(config) {
        Ok(_pid) -> {
          let assert Ok(_) = proxy.start_http(config, clients)

          listen_to_file_changes(config, subject, clients)
        }
        Error(reason) -> {
          logging.error(
            config.logger,
            "Could not start your server:\n" <> reason,
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
    watcher.FilesChanged(changes) -> {
      case has_main_file(config, changes) {
        True -> {
          logging.warning(
            config.logger,
            "Main file changed. You need to restart olive for changes to be applied!",
          )
        }
        False -> {
          let changelog =
            changes
            |> list.map(fn(change) { change.file_name })
            |> string.join("\n")
          logging.debug(config.logger, "Files changed:\n" <> changelog)

          let after_build = case needs_rebuild(changes) {
            True -> server_run.reload_server_code(config)
            False -> Ok(Nil)
          }

          case after_build {
            Ok(_) -> {
              client_registry.trigger(clients)
            }
            Error(msg) -> {
              logging.error(config.logger, msg)
            }
          }
          listen_to_file_changes(config, subject, clients)
        }
      }
    }
  }
}

fn has_main_file(config: config.Config, changes: List(watcher.Change)) {
  let main_file = "src/" <> config.name <> ".gleam"
  changes
  |> list.any(fn(change) { string.ends_with(change.file_name, main_file) })
}

fn needs_rebuild(changes: List(watcher.Change)) {
  changes
  |> list.any(fn(change) {
    case change {
      watcher.SourceChange(..) -> True
      watcher.PrivChange(..) -> False
    }
  })
}
