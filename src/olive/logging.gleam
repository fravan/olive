import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/string

pub opaque type Logger {
  Logger(level: Int)
}

pub type LogLevel {
  None
  Error
  Warning
  Notice
}

pub fn get_logger(level: LogLevel) -> Logger {
  Logger(get_log_level_value(level))
}

pub fn notice(logger: Logger, msg: String) {
  log(logger, Notice, msg)
}

pub fn warning(logger: Logger, msg: String) {
  log(logger, Warning, msg)
}

pub fn error(logger: Logger, msg: String) {
  log(logger, Error, msg)
}

fn log(logger: Logger, level: LogLevel, msg: String) {
  case int.compare(get_log_level_value(level), logger.level) {
    order.Gt | order.Eq -> io.println(get_msg_format(level, msg))
    order.Lt -> Nil
  }
}

fn get_msg_format(level: LogLevel, msg: String) {
  use <- bool.guard(when: level == None, return: "")

  msg
  |> string.split(on: "\n")
  |> list.fold(from: "", with: fn(acc, msg) {
    let formatted_msg = case level {
      None -> ""
      Error ->
        get_formatted_msg(acc, msg, "\u{001b}[31;1mOLIVE - ERROR:\u{001b}[0m ")
      Notice ->
        get_formatted_msg(acc, msg, "\u{001b}[32;1mOLIVE - NOTICE:\u{001b}[0m ")
      Warning ->
        get_formatted_msg(
          acc,
          msg,
          "\u{001b}[33;1mOLIVE - WARNING:\u{001b}[0m ",
        )
    }
    acc <> formatted_msg
  })
}

fn get_formatted_msg(acc: String, msg: String, format: String) {
  let #(new_line_prefix, indent) = case acc {
    "" -> #("", "")
    _ -> #("\n", "\t")
  }
  new_line_prefix <> format <> indent <> msg
}

fn get_log_level_value(level: LogLevel) {
  case level {
    None -> 9000
    Error -> 10
    Warning -> 5
    Notice -> 1
  }
}
