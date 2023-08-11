pub type Value

pub type Schema(t)

@external(erlang, "apollo_t", "new_value")
@external(javascript, "../apollo_t.mjs", "new_value")
fn new_value(t: String) -> Value

pub fn int() -> Value {
  new_value("Int")
}

pub fn string() -> Value {
  new_value("String")
}

pub fn float() -> Value {
  new_value("Float")
}

pub fn bool() -> Value {
  new_value("Bool")
}
