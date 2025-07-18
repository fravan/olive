# olive 🫒

[![Package Version](https://img.shields.io/hexpm/v/olive)](https://hex.pm/packages/olive)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/olive/)

# Table of content
- [Objectives](#objectives)
- [Current limitations](#current-limitations)
- [Installation and usage](#installation-and-usage)
- [Example](#example)
- [Technicalities](#technicalities)

# Objectives

Olive is a development tool to help working on a classic GLEAM server that renders HTML.
Olive takes care of launching your server as well as a proxy to enable live reloading features.

> ⚠️ Olive has some limitations, and is no longer in active development.
> Some feature might still be rough around the edges, be aware!

Make sure to checkout the help for any configuration options with `gleam run -m olive -- --help`

# Current limitations

Current limitations are:

- For the proxy to work, your server needs to answer with a valid html document containing a `<head>` element.
- Olive watches only for `.gleam` file changes under your project `src/` folder, as well as any path dependencies you might have.
- Olive cannot reload any changes made in your main file where the `pub fn main()` is defined. See [Technicalities](#technicalities) for more info.


# Installation and usage

- Add olive has a dev dependency: `gleam add --dev olive`
- Run olive: `gleam run -m olive`

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
    |> mist.start

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


# Technicalities

Olive works by:

1. Spawning your gleam code:
```erlang
spawn_link("<your_project>@@main", run, ["<your_project>"]).
```
With `your_project` being the `name` defined in `gleam.toml`.

2. Launching a proxy that reroutes all requests to your server.
3. Looping on file change events, trigger a new build, a code reload, and sending a websocket message for connected browsers to reload.

## Code reload

Some limitations deny Olive from simply restarting your server on code changes.
(_some processes hang indefinitely instead of being killed when trying to restart everything_)

To counter this, Olive hot reloads any modified modules, using the `code` module of erlang.
This can cause some issue with global state and how updated code works with it.
In case of strange bug, try to restart olive!

This code reload cannot work on your main file because it is an init function that only runs once.
In case you modify it, olive crashes to let you know you _have_ to restart it.

## Proxy

The proxy is dumb for now and does not handle Web sockets nor SSE connections. It waits for the server to end the connection before sending it to the client, so any long lived connection mechanism is broken. I don't know how to create a nicer proxy, and that's the **main** problem remaining on Olive… Everything else seems to work fine 👍
