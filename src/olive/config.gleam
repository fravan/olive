import argv
import clip
import clip/help
import clip/opt
import filepath
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/io
import gleam/option
import gleam/result
import gleam/string
import olive/logging
import simplifile
import tom

pub fn get_name() {
  let assert Ok(config_file) = simplifile.read("gleam.toml")
  let assert Ok(toml) = tom.parse(config_file)
  let assert Ok(name) = tom.get_string(toml, ["name"])

  name
}

pub type Config {
  Config(
    name: String,
    main_port: Int,
    proxy_port: Int,
    bind: String,
    log: logging.LogLevelFilter,
    dirs: List(String),
  )
}

pub fn parse_from_cli() -> option.Option(Config) {
  case read_gleam_toml() {
    Ok(stuff) -> {
      let #(name, dirs) = stuff
      case
        clip.command({
          use main_port <- clip.parameter
          use proxy_port <- clip.parameter
          use bind <- clip.parameter
          use log <- clip.parameter

          Config(name:, main_port:, proxy_port:, bind:, log:, dirs:)
        })
        |> clip.opt(main_port_opt())
        |> clip.opt(proxy_port_opt())
        |> clip.opt(bind_opt())
        |> clip.opt(log_opt())
        |> clip.help(help.simple(
          "olive",
          "Runs a dev proxy for easy live reloading and automatic code changes",
        ))
        |> clip.run(argv.load().arguments)
      {
        Ok(config) -> option.Some(config)
        Error(e) -> {
          logging.error(e)
          option.None
        }
      }
    }
    Error(err) -> {
      logging.error(err)
      option.None
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
      "error" | "errors" | "errorsonly" | "erroronly" -> logging.ErrorsOnly
      "none" | "nologs" -> logging.NoLogs
      _ -> logging.AllLogs
    }
  })
  |> opt.default(logging.AllLogs)
  |> opt.help("Either All, Error or None to filter logs from olive.")
}

@external(erlang, "olive_ffi", "get_cwd")
fn get_cwd() -> Result(String, Atom)

fn read_gleam_toml() -> Result(#(String, List(String)), String) {
  logging.notice(
    "Reading gleam.toml to retrieve project's name and directories to watch",
  )
  get_gleam_toml_path()
  |> result.then(fn(path) {
    path
    |> simplifile.read
    |> result.map_error(simplifile_error_to_string)
  })
  |> result.then(fn(file_content) {
    file_content
    |> tom.parse
    |> result.map_error(tom_error_to_string)
  })
  |> result.then(fn(toml) {
    let name =
      tom.get_string(toml, ["name"])
      |> result.map_error(tom_field_error_to_string)
    // want to read InlineTable with a dict that has a key "path"
    let dependencies =
      tom.get_table(toml, ["dependencies"])
      |> result.map_error(tom_field_error_to_string)
      |> result.map(fn(deps) {
        deps
        |> dict.fold(from: ["src"], with: fn(acc, _key, value) {
          case value {
            tom.InlineTable(table) -> {
              case dict.get(table, "path") {
                Error(_) -> acc
                Ok(tom.String(path)) -> [path, ..acc]
                Ok(_) -> acc
              }
            }
            _ -> acc
          }
        })
      })

    case name, dependencies {
      Error(e1), Error(e2) -> Error(e1 <> "\n" <> e2)
      Error(e), _ -> Error(e)
      _, Error(e) -> Error(e)
      Ok(name), Ok(deps) -> Ok(#(name, deps))
    }
  })
}

// fn parse_file(file_content: List(String), name: option.Option(String), dirs: List(String)) {
//   case file_content {
//     [] -> #(name, dirs)
//     [line, ..rest] -> {
//       let assert Ok(reg_name) = regexp.from_string("name ?= ?\"(.*)\"")
//     }
//   }
// }

fn tom_field_error_to_string(parse_error: tom.GetError) {
  case parse_error {
    tom.NotFound(keys) ->
      "Could not find " <> string.join(keys, ".") <> " in gleam.toml"
    tom.WrongType(keys, expected, got) ->
      "Could not read "
      <> string.join(keys, ".")
      <> ". Expected "
      <> expected
      <> " but got "
      <> got
  }
}

fn tom_error_to_string(parse_error: tom.ParseError) -> String {
  case parse_error {
    tom.KeyAlreadyInUse(keys) ->
      "Could not parse gleam.toml: Key already in use: "
      <> string.join(keys, ", ")
    tom.Unexpected(char, _) ->
      "Could not parse gleam.toml: Unexpected character: " <> char
  }
}

fn simplifile_error_to_string(err: simplifile.FileError) {
  "Could not read gleam.toml: " <> simplifile.describe_error(err)
}

fn get_gleam_toml_path() {
  get_cwd()
  |> result.map_error(fn(posix) {
    "Could not get current working dir, error is: " <> atom.to_string(posix)
  })
  |> result.then(get_root)
}

fn get_root(path: String) {
  let gleam_path = filepath.join(path, "gleam.toml")
  case simplifile.is_file(gleam_path) {
    Ok(True) -> Ok(gleam_path)
    Ok(False) | Error(_) -> {
      case filepath.expand(filepath.join(path, "..")) {
        Ok(path) -> get_root(path)
        Error(_) -> Error("Could not locate root dir where gleam.toml is")
      }
    }
  }
}
