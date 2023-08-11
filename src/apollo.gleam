import apollo/t

/// Takes in a schema from `typed` along with a mapping and returns a result of the type.
/// # Example
///     let res: t.Schema(Id) = typed(...)
///     let assert Ok(my_id) = validate(res, [#("id", "1532594324")])
///     // type of `my_id` is `Id`
@external(erlang, "ffi_apollo", "validate")
@external(javascript, "./ffi_apollo.mjs", "validate")
pub fn validate(schema: t.Schema(type_), obj: unknown) -> Result(type_, String)

/// Returns a schema to be used by `validate`. Takes in a type constructor and a list of key, value pairs.
/// # Example
///     type Id {
///       Id(id: String)
///     }
///     let res: t.Schema(Id) = typed(
///         Id, 
///         #(
///             #("id", t.string())
///         )
///     )
@external(erlang, "ffi_apollo", "typed")
@external(javascript, "./ffi_apollo.mjs", "typed")
pub fn typed(type_: a, schema: List(#(String, t.Value))) -> t.Schema(b)
