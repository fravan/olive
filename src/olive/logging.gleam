import gleam/int
import gleam/io
import gleam/order

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
  case level {
    None -> ""
    Error -> "\u{001b}[31;1mOLIVE - ERROR:\u{001b}[0m " <> msg
    Notice -> "\u{001b}[32;1mOLIVE - NOTICE:\u{001b}[0m " <> msg
    Warning -> "\u{001b}[33;1mOLIVE - WARNING:\u{001b}[0m " <> msg
  }
}

fn get_log_level_value(level: LogLevel) {
  case level {
    None -> 9000
    Error -> 10
    Warning -> 5
    Notice -> 1
  }
}
