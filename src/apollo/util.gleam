/// Used when making custon filters to check the type of a value
pub type AnyType {
  IntT(Int)
  StringT(String)
  FloatT(Float)
  BoolT(Bool)
}

/// Shorthand for converting a Boolean to a Result
pub fn to_err(b: Bool, err: String) -> Result(Nil, String) {
  case b {
    True -> Ok(Nil)
    False -> Error(err)
  }
}
