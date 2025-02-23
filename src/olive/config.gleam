import filepath
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/result
import gleam/string
import olive/cli
import olive/logging
import simplifile
import tom

/// Holds all the important bits to run olive
pub type Config {
  Config(
    /// The main port needs to match the one listen by the main server
    main_port: Int,
    /// The proxy port is used by olive to create the web proxy server
    proxy_port: Int,
    /// The bind address needs to match the one used by the main server
    /// and is used by the proxy to replicate the same behaviour
    bind: String,
    /// The root dir, where gleam.toml lives
    root: String,
    /// The name of the project (field "name" in gleam.toml)
    name: String,
    /// The list of dirs olive needs to watch to trigger live reload
    dirs: List(String),
  )
}

@external(erlang, "olive_ffi", "get_cwd")
fn get_cwd() -> Result(String, Atom)

pub fn read_config(options: cli.CliOptions) -> Result(Config, String) {
  logging.notice(
    "Reading gleam.toml to retrieve project's name and directories to watch",
  )

  use root <- result.try(get_gleam_toml_path())
  use gleam_toml <- result.try(read_gleam_toml(root))
  use name <- result.try(read_project_name(gleam_toml))
  use deps <- result.try(read_project_dependencies(gleam_toml, root))

  logging.notice(
    "Olive will watch for changes in those folders:\n"
    <> string.join(deps, "\n"),
  )

  Ok(Config(
    root:,
    name:,
    dirs: deps,
    bind: options.bind,
    main_port: options.main_port,
    proxy_port: options.proxy_port,
  ))
}

type TomlFile =
  dict.Dict(String, tom.Toml)

fn read_project_name(toml: TomlFile) {
  tom.get_string(toml, ["name"]) |> result.map_error(tom_field_error_to_string)
}

fn read_project_dependencies(toml: TomlFile, root: String) {
  // We want path dependencies only.
  // Path dependencies such as `common = { path = "../common" }` will be
  // watched at "../common/src" as well as the main src folder.
  tom.get_table(toml, ["dependencies"])
  |> result.map_error(tom_field_error_to_string)
  |> result.map(fn(deps) {
    deps
    |> dict.fold(from: [filepath.join(root, "src")], with: fn(acc, _key, value) {
      case value {
        tom.InlineTable(table) -> {
          case dict.get(table, "path") {
            Error(_) -> acc
            Ok(tom.String(path)) -> [get_full_path(root, path), ..acc]
            Ok(_) -> acc
          }
        }
        _ -> acc
      }
    })
  })
}

fn get_gleam_toml_path() -> Result(String, String) {
  get_cwd()
  |> result.map_error(cwd_error_to_string)
  |> result.then(get_root)
}

fn get_root(path: String) -> Result(String, String) {
  case simplifile.is_file(filepath.join(path, "gleam.toml")) {
    Ok(True) -> Ok(path)
    Ok(False) | Error(_) -> {
      case filepath.expand(filepath.join(path, "..")) {
        Ok(path) -> get_root(path)
        Error(_) -> Error("Could not locate root dir where gleam.toml is")
      }
    }
  }
}

fn get_full_path(root: String, path: String) {
  // path comes from gleam.toml so we can probably assert with good knowlegde
  // `gleam build` would not compile if the path was wrong anyway…
  let assert Ok(full_path) =
    filepath.join(root, path)
    |> filepath.join("src")
    |> filepath.expand
  full_path
}

fn read_gleam_toml(path: String) -> Result(TomlFile, String) {
  filepath.join(path, "gleam.toml")
  |> simplifile.read
  |> result.map_error(simplifile_error_to_string)
  |> result.try(fn(content) {
    tom.parse(content)
    |> result.map_error(tom_error_to_string)
  })
}

// ERROR MAPPERS -------------------------------------------------

fn cwd_error_to_string(posix_error: Atom) -> String {
  "Could not get current working dir, error is: " <> atom.to_string(posix_error)
}

fn simplifile_error_to_string(err: simplifile.FileError) -> String {
  "Could not read gleam.toml: " <> simplifile.describe_error(err)
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

fn tom_field_error_to_string(parse_error: tom.GetError) -> String {
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
