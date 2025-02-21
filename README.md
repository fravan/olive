# olive 🫒

[![Package Version](https://img.shields.io/hexpm/v/olive)](https://hex.pm/packages/olive)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/olive/)

# Objectives

Olive is a development tool to help working on a wisp server (AKA the _main server_ in this doc).
Olive is a proxy to your _main server_, that adds the following features:

- Hot reload of any modified erlang modules in your _main server_
- Live reload of connected browsers to the olive proxy


The different pieces are:
- A proxy to connect to instead of directly to the _main server_
- Said proxy injects a live reload javascript snippet and handles a websocket connection to trigger connected browsers
- Olive listens for file changes in your source code and:
  1. Triggers a new build when code changes (`gleam build`)
  2. Hot reload the modified erlang modules of your _main server_ (`erlang code:atomic_load/1`)
  3. Sends a websocket message to connected browsers so they can reload

Anything else sent to the proxy is directly rerouted to your _main server_.

## Who is this for?
- ❌ If you are using lustre, checkout lustre dev tools instead!
- ❌ If you are developping an API server, you might be better off with a simple `watchexec`
- ✅ If you are working on a server that renders HTML (vanilla, htmx or others), this might interest you!

# Caveats / Current limitations
For now, the tool is very much a Proof of Concept.
As such, the current limitations are:

- The proxy is on port 1234
- The proxy reroutes to port 3000 (so it forces your _main server_ to listen to 3000)
- Your _main server_ needs to answer with a valid html document and a `</head>` at some point.
  Olive uses that to inject a piece of javascript for the websocket to connect.
- Olive must run in the root dir where `gleam.toml` is so it can get the name of the project to run it.
- Olive speaks (logs) and there is no way to make it quiet for now
- Olive only watches for changes under `src/` and nothing else
- All of the above is not configurable but might be in the future!


Because of the nature of the `code:atomic_load`, any changes to your `main` function will not be taken into account.
Basically, the gleam file ran by the default `gleam run`, which should be a wisp server,
will never be replaced because it is always running. The BEAM never has a chance to swap for the new module.
In this case, you have to kill olive and rerun it.

# Installation and usage

Add  olive has a dev dependency of your _main server_:

```sh
gleam add --dev olive@1
```

Then, use gleam to run it:
```sh
gleam run -m olive
```

Olive takes care of running your _main server_ for you!

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
  |> response.set_body(mist.Bytes(bytes_tree.from_string(html("Hello world"))))
}

fn html(content: String) {
  "<html>
    <head>
      <title>My project</title>
    </head>
    <body>"
    <> content
    <> "</body>
    </html>"
}
```

Now, after installing `olive`, you can run `gleam run -m olive`, and the following will happen:
1. Your server will be listening on localhost:3000
2. The olive server will be listening on localhost:1234

Open localhost:1234, and you should be granted with a `Hello world`.

Now update the file in `router.gleam` so the server sends back `Hello olive`, and voila!
Your browser should refresh automatically after a quick rebuild and show you the new message 🎉

Any updates to `src/my_project.gleam` will not work, as explained in the Caveats chapter.
