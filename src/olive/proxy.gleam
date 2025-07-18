//// The `proxy` module starts the proxy server that redirects to the "main" server
//// It also injects a websocket connection and handles it, via the `client_registry`
//// module, to add / remove them, and send them a websocket message to trigger the
//// actual reload of the browser.

import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/option
import gleam/result
import gleam/string
import mist
import olive/client_registry.{type ClientRegistry}
import olive/config
import olive/logging
import simplifile
import wisp

pub fn start_http(config: config.Config, clients: ClientRegistry) {
  fn(req: request.Request(mist.Connection)) -> response.Response(
    mist.ResponseData,
  ) {
    case request.path_segments(req) {
      ["ws_livereload"] -> handle_websocket(req, clients, config)
      _ -> handle_request(config, req)
    }
  }
  |> mist.new
  |> mist.bind(config.bind)
  |> mist.port(config.proxy_port)
  |> mist.start
}

fn handle_websocket(
  req: request.Request(mist.Connection),
  clients: ClientRegistry,
  config: config.Config,
) {
  mist.websocket(
    request: req,
    on_init: fn(_connection) {
      let client = process.new_subject()
      client_registry.add(clients, client)

      let selector =
        process.new_selector()
        |> process.select(client)

      #(client, option.Some(selector))
    },
    on_close: fn(client) { client_registry.remove(clients, client) },
    handler: fn(client, message, conn) {
      case message {
        mist.Custom(client_registry.Reload) -> {
          case mist.send_text_frame(conn, "reload") {
            Ok(_) -> {
              logging.notice(
                config.logger,
                "Successfully sent reload message to client",
              )
              mist.continue(client)
            }
            Error(_) -> {
              logging.error(config.logger, "Could not send message to client")
              mist.stop()
            }
          }
        }
        mist.Closed | mist.Shutdown -> {
          logging.warning(config.logger, "Client has disconnected unexpectedly")
          client_registry.remove(clients, client)
          mist.stop()
        }
        mist.Text(_) | mist.Binary(_) -> {
          mist.continue(client)
        }
      }
    },
  )
}

fn handle_request(config: config.Config, req: request.Request(mist.Connection)) {
  let internal_error =
    response.new(500)
    |> response.set_body(mist.Bytes(bytes_tree.new()))

  let assert Ok(req) = mist.read_body(req, 100 * 1024 * 1024)
  request.Request(..req, port: option.Some(config.main_port))
  |> httpc.send_bits
  |> result.map(response.map(_, maybe_inject_ws))
  |> result.map(response.map(_, bytes_tree.from_bit_array))
  |> result.map(response.map(_, mist.Bytes))
  |> result.unwrap(internal_error)
}

fn maybe_inject_ws(response: BitArray) {
  case bit_array.to_string(response) {
    Ok(str) -> bit_array.from_string(inject(str))
    Error(_) -> response
  }
}

fn inject(html: String) -> String {
  let assert Ok(priv_dir) = wisp.priv_directory("olive")
  let assert Ok(file_content) = simplifile.read(priv_dir <> "/proxy.js")
  let script = "<script>" <> file_content <> "</script>"

  html
  |> string.replace("</head>", script <> "</head>")
}
