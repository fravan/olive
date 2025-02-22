pub type LogLevel {
  Emergency
  Error
  Debug
}

pub type LogLevelFilter {
  AllLogs
  ErrorsOnly
  NoLogs
}

pub fn configure_logs(level: LogLevelFilter) -> Nil {
  // Will only show logs that are equal or greater than given level
  case level {
    AllLogs -> erlang_configure_logs(Debug)
    ErrorsOnly -> erlang_configure_logs(Error)
    NoLogs -> erlang_configure_logs(Emergency)
  }
}

@external(erlang, "olive_ffi", "configure_logs")
fn erlang_configure_logs(level: LogLevel) -> Nil

pub fn debug(msg: String) {
  erlang_log(Debug, msg)
}

pub fn error(msg: String) {
  erlang_log(Error, msg)
}

@external(erlang, "olive_ffi", "log")
fn erlang_log(level: LogLevel, message: String) -> Nil
