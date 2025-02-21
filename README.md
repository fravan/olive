# dev_server

[![Package Version](https://img.shields.io/hexpm/v/dev_server)](https://hex.pm/packages/dev_server)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/dev_server/)

# Objectives

The dev_server is a development tool to help working on a wisp server (AKA the _main server_ in this doc).
The dev_server is a proxy to your _main server_, that adds the following features:

- Hot reload of any modified erlang modules in your _main server_
- Live reload of connected browsers to the dev_server proxy


The different pieces are:
- A proxy to connect to instead of directly to the _main server_
- Said proxy injects a live reload javascript snippet and handles a websocket connection to trigger connected browsers
- The dev_server listens for file changes in your source code and:
  1. Triggers a new build when code changes (`gleam build`)
  2. Hot reload the modified erlang modules of your _main server_ (`erlang code:atomic_load/1`)
  3. Sends a websocket message to connected browsers so they can reload

Anything else sent to the proxy is directly rerouted to your _main server_.

## Who is this for?
If you are using lustre -> use lustre dev tools instead!

This package is for people working on a wisp server that renders HTML. Some good old days server, HTMX flavored stuff and such!

# Caveats / Current limitations
Because of the nature of the `code:atomic_load`, any changes to your `main` function will not be taken into account.
Basically, the gleam file ran by the default `gleam run`, which should be a wisp server,
will never be replaced because it is always running. The BEAM never has a chance to swap for the new module.
In this case, you have to kill the dev_server and rerun it.

# Installation and usage

Add the dev_server has a dev dependency of your _main server_:

```sh
gleam add --dev dev_server@1
```

Then, use gleam to run the dev_server:
```sh
gleam run -m dev_server
```

# Example

This is a stripped example to show the relevant parts only. Be sure to check the docs for wisp / mist / any other lib that allows to have a server running on the BEAM.

Let's say you did a `gleam new my_project`.

In `src/my_project.gleam`:
```gleam
pub fn main() {
  wisp.configure_logger()

  let assert Ok(_) =
    wisp_mist.handler(router.handle_request, "secret_key")
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http

  process.sleep_forever()
}
```
In `src/my_project/router.gleam`:
```gleam
pub fn handle_request(_req: Request) -> Response {
  response.new(200)
  |> response.set_body(mist.Bytes(bytes_tree.from_string("Hello world")))
}
```

Now, after installing `dev_server`, you can run `gleam run -m dev_server`, and the following will happen:
1. Your server will be listening on localhost:3000
2. The dev server (proxy) will be listening on localhost:1234

Open the proxy, and you should be granted with a `Hello world`.

Now update the file in `router.gleam` so you response with `Hello proxy`, and voila!
Your browser should refresh automatically after a quick rebuild and show you the new message :)

Any updates to `src/my_project.gleam` will not work, as explained in the Caveats chapter.
