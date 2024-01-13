import gleeunit
import gleeunit/should
import apollo.{typed, validate}
import apollo/t

pub fn main() {
  gleeunit.main()
}

type Basic {
  Basic(str_f: String, float_f: Float, int_f: Int, bool_f: Bool)
}

pub fn type_basic_test() {
  typed(Basic, [
    #("str_f", t.string()),
    #("float_f", t.float()),
    #("int_f", t.int()),
    #("bool_f", t.bool()),
  ])
  |> validate(#(
    #("str_f", "abc"),
    #("float_f", 1.5),
    #("int_f", 213),
    #("bool_f", True),
  ))
  |> should.be_ok()
}

pub fn type_err_basic_test() {
  typed(Basic, [
    #("str_f", t.string()),
    #("float_f", t.float()),
    #("int_f", t.int()),
    #("bool_f", t.bool()),
  ])
  |> validate(#(
    #("str_f", "abc"),
    #("float_f", "a string"),
    #("int_f", 213),
    #("bool_f", True),
  ))
  |> should.be_error()
}
