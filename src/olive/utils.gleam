import gleam/option

pub fn from_option(opt: option.Option(a), cb: fn(a) -> Nil) -> Nil {
  case opt {
    option.Some(opt) -> cb(opt)
    option.None -> Nil
  }
}
