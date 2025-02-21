//// The `proxy` module starts the proxy server that redirects to the "main" server
//// It also injects a websocket connection and handles it, via the `client_registry`
//// module, to add / remove them, and send them a websocket message to trigger the
//// actual reload of the browser.

import dev_server/client_registry.{type ClientRegistry}
import dev_server/logging
import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/process
import gleam/function
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/option
import gleam/otp/actor
import gleam/result
import gleam/string
import mist

pub fn start_http(clients: ClientRegistry) {
  fn(req: request.Request(mist.Connection)) -> response.Response(
    mist.ResponseData,
  ) {
    case request.path_segments(req) {
      ["ws_livereload"] -> handle_websocket(req, clients)
      _ -> handle_request(req)
    }
  }
  |> mist.new
  |> mist.port(1234)
  |> mist.start_http
}

fn handle_websocket(
  req: request.Request(mist.Connection),
  clients: ClientRegistry,
) {
  mist.websocket(
    request: req,
    on_init: fn(_connection) {
      let client = process.new_subject()
      client_registry.add(clients, client)

      let selector =
        process.new_selector()
        |> process.selecting(client, function.identity)

      #(client, option.Some(selector))
    },
    on_close: fn(client) { client_registry.remove(clients, client) },
    handler: fn(client, conn, message) {
      case message {
        mist.Custom(client_registry.Reload) -> {
          case mist.send_text_frame(conn, "reload") {
            Ok(_) -> {
              logging.log_debug("Successfully sent reload message to client")
              actor.continue(client)
            }
            Error(_) -> {
              logging.log_error("Could not send message to client")
              actor.Stop(process.Normal)
            }
          }
        }
        mist.Closed | mist.Shutdown -> {
          logging.log_debug("Client has disconnected")
          client_registry.remove(clients, client)
          actor.Stop(process.Normal)
        }
        mist.Text(_) | mist.Binary(_) -> {
          actor.continue(client)
        }
      }
    },
  )
}

fn handle_request(req: request.Request(mist.Connection)) {
  let internal_error =
    response.new(500)
    |> response.set_body(mist.Bytes(bytes_tree.new()))

  let assert Ok(req) = mist.read_body(req, 100 * 1024 * 1024)
  request.Request(..req, port: option.Some(3000))
  |> httpc.send_bits
  |> result.map(response.map(_, maybe_inject_sse))
  |> result.map(response.map(_, bytes_tree.from_bit_array))
  |> result.map(response.map(_, mist.Bytes))
  |> result.unwrap(internal_error)
}

fn maybe_inject_sse(response: BitArray) {
  case bit_array.to_string(response) {
    Ok(str) -> bit_array.from_string(inject(str))
    Error(_) -> response
  }
}

fn inject(html: String) -> String {
  let script =
    "<script>
    let liveReloadWebSocket = null;
    let reconnectTimeout = null;

    function connect() {
      clearTimeout(reconnectTimeout);
      liveReloadWebSocket = new WebSocket(`ws://${window.location.host}/ws_livereload`);

      liveReloadWebSocket.onmessage = (event) => {
        window.location.reload();
      };
      liveReloadWebSocket.onclose = reconnect;
      liveReloadWebSocket.onerror = reconnect;
    }

    function reconnect() {
      clearTimeout(reconnectTimeout);
      reconnectTimeout = setTimeout(connect, 5000);
    }
    connect();
  </script>"

  html
  |> string.replace("</head>", script <> "</head>")
}
