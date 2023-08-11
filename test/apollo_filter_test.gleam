import gleeunit
import gleeunit/should
import gleam/regex
import apollo.{typed, validate}
import apollo/t
import apollo/filter as f

pub fn main() {
  gleeunit.main()
}

type Small {
  Small(str_f: String, int_f: Int)
  Smol(str_f: String)
}

pub fn length_test() {
  typed(
    Small,
    [
      #(
        "str_f",
        t.string()
        |> f.length(4),
      ),
      #(
        "int_f",
        t.int()
        |> f.length(2),
      ),
    ],
  )
  |> validate(#(#("str_f", "abcd"), #("int_f", 21)))
  |> should.be_ok()
}

pub fn length_err_test() {
  typed(
    Small,
    [
      #(
        "str_f",
        t.string()
        |> f.length(5),
      ),
      #(
        "int_f",
        t.int()
        |> f.length(2),
      ),
    ],
  )
  |> validate(#(#("str_f", "abcd"), #("int_f", 21)))
  |> should.be_error()
}

pub fn min_test() {
  typed(
    Smol,
    [
      #(
        "str_f",
        t.string()
        |> f.min(4),
      ),
    ],
  )
  |> validate(#(#("str_f", "abcdefg")))
  |> should.be_ok()
}

pub fn min_err_test() {
  typed(
    Smol,
    [
      #(
        "str_f",
        t.string()
        |> f.min(4),
      ),
    ],
  )
  |> validate(#(#("str_f", "abc")))
  |> should.be_error()
}

pub fn max_test() {
  typed(
    Smol,
    [
      #(
        "str_f",
        t.string()
        |> f.max(4),
      ),
    ],
  )
  |> validate(#(#("str_f", "ab")))
  |> should.be_ok()
}

pub fn max_err_test() {
  typed(
    Smol,
    [
      #(
        "str_f",
        t.string()
        |> f.max(4),
      ),
    ],
  )
  |> validate(#(#("str_f", "abcdefg")))
  |> should.be_error()
}

const pattern = "[gG]leam"

pub fn regex_test() {
  let assert Ok(re) = regex.compile(pattern, with: regex.Options(False, False))

  typed(
    Smol,
    [
      #(
        "str_f",
        t.string()
        |> f.regex(re),
      ),
    ],
  )
  |> validate(#(#("str_f", "gleam")))
  |> should.be_ok()
}

pub fn regex_err_test() {
  let assert Ok(re) = regex.compile(pattern, with: regex.Options(False, False))

  typed(
    Smol,
    [
      #(
        "str_f",
        t.string()
        |> f.regex(re),
      ),
    ],
  )
  |> validate(#(#("str_f", "gleem")))
  |> should.be_error()
}
