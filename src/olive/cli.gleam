//// The `cli` module holds the list of options the user can pass when launching olive.

import argv
import clip
import clip/help
import clip/opt
import gleam/io
import gleam/string
import olive/logging

pub type CliOptions {
  CliOptions(
    main_port: Int,
    proxy_port: Int,
    bind: String,
    log: logging.LogLevel,
    debounce_in_ms: Int,
  )
}

pub fn get_options() -> Result(CliOptions, Nil) {
  case
    clip.command({
      use main_port <- clip.parameter
      use proxy_port <- clip.parameter
      use bind <- clip.parameter
      use log <- clip.parameter
      use debounce_in_ms <- clip.parameter

      CliOptions(main_port:, proxy_port:, bind:, log:, debounce_in_ms:)
    })
    |> clip.opt(main_port_opt())
    |> clip.opt(proxy_port_opt())
    |> clip.opt(bind_opt())
    |> clip.opt(log_opt())
    |> clip.opt(debounce_in_ms_opt())
    |> clip.help(help.simple(
      "olive",
      "Runs a dev proxy for easy live reloading and automatic code changes",
    ))
    |> clip.run(argv.load().arguments)
  {
    Ok(options) -> Ok(options)
    Error(something_to_show) -> {
      // We don't use the logger here because we want to actually print to the console
      // this is for the --help or any error while parsing args.
      // Also, at this point the logger is not configured yet
      // (cause we don't know what kind of filter to apply)
      io.println(something_to_show)
      Error(Nil)
    }
  }
}

fn main_port_opt() {
  opt.new("main_port")
  |> opt.int
  |> opt.default(3000)
  |> opt.help("The port your main server listens to.")
}

fn proxy_port_opt() {
  opt.new("proxy_port")
  |> opt.int
  |> opt.default(1234)
  |> opt.help("The port the proxy listens to, handled by olive.")
}

fn bind_opt() {
  opt.new("bind")
  |> opt.default("localhost")
  |> opt.help("The proxy binding address.")
}

fn log_opt() {
  opt.new("log")
  |> opt.map(fn(v) {
    case string.lowercase(v) {
      "error" | "errors" -> logging.Error
      "warning" | "warnings" -> logging.Warning
      "none" | "nologs" -> logging.None
      "debug" -> logging.Debug
      _ -> logging.Notice
    }
  })
  |> opt.default(logging.Notice)
  |> opt.help(
    "Either None, Error, Warning, Notice or Debug to filter logs from olive.",
  )
}

fn debounce_in_ms_opt() {
  opt.new("watch_debounce")
  |> opt.int
  |> opt.default(50)
  |> opt.help(
    "Debounce in ms used before triggering a rebuild and client reload.
    Useful when you have multiple changes in a short period of time.",
  )
}
