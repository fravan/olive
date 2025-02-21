import gleam/io

pub fn log_debug(msg: String) {
  io.println("\u{001b}[38;5;2mDEV_SERVER\u{001b}[0m " <> msg)
}

pub fn log_error(msg: String) {
  io.println("\u{001b}[38;5;1mDEV_SERVER\u{001b}[0m " <> msg)
}
