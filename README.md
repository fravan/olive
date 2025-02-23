# olive 🫒

[![Package Version](https://img.shields.io/hexpm/v/olive)](https://hex.pm/packages/olive)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/olive/)

# Table of content
- [Objectives](#objectives)
  - [Who is this for?](#who-is-this-for)
- [Caveats / Current limitations](#caveats--current-limitations)
- [Installation and usage](#installation-and-usage)
- [Example](#example)
- [Logging](#logging)
- [Technicalities](#technicalities)

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

> Make sure to checkout the help for any configuration options with `gleam run -m olive -- --help`

Current limitations are:

- Your _main server_ needs to answer with a valid html document and a `</head>` at some point.
  Olive uses that to inject a piece of javascript for the websocket to connect.
- Olive watches only for `.gleam` file changes under your project `src/` folder, as well as any path dependencies you might have.
- Olive cannot reload any changes made in your main file where the `pub fn main()` is defined. See [Technicalities](#technicalities) for more info.


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


# Logging

Olive uses erlang `logger` module, and allows you to specify what goes out on your terminal.
3 levels of filters are specified:
- **All**: Will show all olive logs (the default).
- **Error**: Will only show olive errors.
- **None**: Will shutdown olive logs entirely.

Olive logs 2 kind of log level: *Notice* and *Error*.
It also adds the metadata `{domain => [olive]}` to any logs so you can add your own handler on it if needed.

> ⚠️ I don't recommand turning off *Error* log level. ⚠️
>
> When rebuilding your project, any error from `gleam build` will be an Olive error log, and you won't
> see it if it is turned off!

# Technicalities

After building your project, Olive runs your program for you, similar to how `gleam run` would.

In technical words, it means Olive runs the following in erlang world:
```erlang
spawn_link("<your_project>@@main", run, ["<your_project>"]).
```
With `your_project` being the `name` defined in `gleam.toml`.


The reload part lives also in erlang world, using this nice piece of code:
```erlang
reload_modules() ->
    Modules = code:modified_modules(),
    lists:foreach(fun(Mod) -> code:purge(Mod) end, Modules),
    atomic_load(Modules).
```
This purges any modified modules that a `gleam build` would have produced, then replaces them on the fly.

Next time one of this module is called, the new code will take the lead.
This has one inconvenient, your main file never gets called more than once, so even if it gets replaced, it will always be the old code still running.
Most of the time, the main file has a `process.sleep_forever()` so the old code stays.

This is why any changes to your main file will need a reboot of olive.
