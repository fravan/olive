import filepath
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import olive/cli
import olive/logging
import simplifile
import tom

pub type Directory {
  SourceDirectory(path: String)
  PrivDirectory(path: String)
}

/// Holds all the important bits to run olive
pub type Config {
  Config(
    logger: logging.Logger,
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
    dirs: List(Directory),
    /// The debounce in ms to use for file watchers
    debounce_in_ms: Int,
  )
}

@external(erlang, "olive_ffi", "get_cwd")
fn get_cwd() -> Result(String, Atom)

pub fn read_config(
  logger: logging.Logger,
  options: cli.CliOptions,
) -> Result(Config, String) {
  logging.notice(
    logger,
    "Reading gleam.toml to retrieve project's name and directories to watch",
  )

  use root <- result.try(get_gleam_toml_path())
  use gleam_toml <- result.try(read_gleam_toml(root))
  use name <- result.try(read_project_name(gleam_toml))
  use deps <- result.try(read_project_dependencies(gleam_toml, root))

  logging.debug(
    logger,
    "Olive will watch for changes in those folders:\n" <> print_deps(deps),
  )

  Ok(Config(
    logger:,
    root:,
    name:,
    dirs: deps,
    bind: options.bind,
    main_port: options.main_port,
    proxy_port: options.proxy_port,
    debounce_in_ms: options.debounce_in_ms,
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
  let source_directory = check_source_directory(root)
  let priv_directory = check_priv_directory(root)

  tom.get_table(toml, ["dependencies"])
  |> result.map_error(tom_field_error_to_string)
  |> result.map(fn(deps) {
    deps
    |> dict.fold(
      from: [source_directory, priv_directory],
      with: fn(acc, _key, value) {
        case value {
          tom.InlineTable(table) -> {
            case dict.get(table, "path") {
              Error(_) -> acc
              Ok(tom.String(path)) -> {
                let full_path = filepath.join(root, path)
                let source_dir = check_source_directory(full_path)
                let priv_dir = check_priv_directory(full_path)
                [source_dir, priv_dir, ..acc]
              }
              Ok(_) -> acc
            }
          }
          _ -> acc
        }
      },
    )
    |> option.values
  })
}

fn check_source_directory(root: String) {
  check_directory(filepath.join(root, "src"), SourceDirectory)
}

fn check_priv_directory(root: String) {
  check_directory(filepath.join(root, "priv"), PrivDirectory)
}

fn check_directory(path: String, ctor: fn(String) -> Directory) {
  case filepath.expand(path) {
    Ok(full_path) -> {
      case simplifile.is_directory(full_path) {
        Ok(True) -> option.Some(ctor(full_path))
        Ok(False) | Error(_) -> option.None
      }
    }
    Error(Nil) -> option.None
  }
}

fn get_gleam_toml_path() -> Result(String, String) {
  get_cwd()
  |> result.map_error(cwd_error_to_string)
  |> result.try(get_root)
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

fn read_gleam_toml(path: String) -> Result(TomlFile, String) {
  filepath.join(path, "gleam.toml")
  |> simplifile.read
  |> result.map_error(simplifile_error_to_string)
  |> result.try(fn(content) {
    tom.parse(content)
    |> result.map_error(tom_error_to_string)
  })
}

fn print_deps(deps: List(Directory)) {
  string.join(
    list.map(deps, fn(dir) {
      case dir {
        PrivDirectory(path) -> "Any changes in: " <> path
        SourceDirectory(path) -> "Code changes in: " <> path
      }
    }),
    "\n",
  )
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
