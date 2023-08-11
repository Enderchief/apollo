//// Create and use filters to narrow down on types

import gleam/string as std_string
import gleam/int as std_int
import gleam/regex
import apollo/t.{Value}
import apollo/util.{AnyType, IntT, StringT, to_err}

/// Create a custom filter
/// # Example
///     pub fn is_gleam(v: Value) {
///         custom(
///           v,
///           fn(value: AnyType) {
///             case value {
///               StringT(t) -> to_err(t == "Gleam", "Uh oh, expecting the word Gleam")
///               _ -> Error("Type mismatch, expecting String")
///             }
///           }
///         )
///      }
@external(erlang, "apollo_t", "custom")
@external(javascript, "../apollo_t.mjs", "custom")
pub fn custom(v: Value, validator: fn(AnyType) -> Result(Nil, String)) -> Value

/// Shorthand to create a custom filter for only string types
/// # Example
///     pub fn is_gleam(v: Value) {
///         custom_string(
///           v,
///           fn(t: String) {
///             to_err(t == "Gleam", "Uh oh, expecting the word Gleam")
///           }
///         )
///      }
pub fn custom_string(
  v: Value,
  validator: fn(String) -> Result(Nil, String),
) -> Value {
  custom(
    v,
    fn(value: AnyType) {
      case value {
        StringT(t) -> validator(t)
        _ -> Error("Type mismatch, expecting String or Integer")
      }
    },
  )
}

pub fn length(v: Value, length l: Int) -> Value {
  custom(
    v,
    fn(value: AnyType) {
      case value {
        StringT(t) -> {
          to_err(
            std_string.length(t) == l,
            "Length of \"" <> t <> "\" is not equal to " <> std_int.to_string(l),
          )
        }
        IntT(t) -> {
          let stringed_value = std_int.to_string(t)
          to_err(
            std_string.length(stringed_value) == l,
            "Length of \"" <> stringed_value <> "\" is not equal to " <> std_int.to_string(
              l,
            ),
          )
        }
        _ -> Error("Type mismatch, expecting String or Integer")
      }
    },
  )
}

pub fn min(v: Value, length l: Int) -> Value {
  custom(
    v,
    fn(value: AnyType) {
      case value {
        StringT(t) -> {
          to_err(
            std_string.length(t) >= l,
            "Length of \"" <> t <> "\" is less than than " <> std_int.to_string(
              l,
            ),
          )
        }
        _ -> Error("Type mismatch, expecting String")
      }
    },
  )
}

pub fn max(v: Value, length l: Int) -> Value {
  custom_string(
    v,
    fn(t: String) {
      to_err(
        std_string.length(t) <= l,
        "Length of \"" <> t <> "\" is greater than " <> std_int.to_string(l),
      )
    },
  )
}

pub fn regex(v: Value, regex re: regex.Regex) -> Value {
  custom_string(
    v,
    fn(t: String) {
      to_err(
        regex.check(re, t),
        "\"" <> t <> "\" not matched by the regular expression",
      )
    },
  )
}
