import simplifile
import tom

pub fn get_name() {
  let assert Ok(config_file) = simplifile.read("gleam.toml")
  let assert Ok(toml) = tom.parse(config_file)
  let assert Ok(name) = tom.get_string(toml, ["name"])

  name
}
