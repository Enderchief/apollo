# Apollo

[![Package Version](https://img.shields.io/hexpm/v/apollo)](https://hex.pm/packages/apollo)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/apollo/)

A validation library named after the God of truth and prophecy.

## An Basic Example

```gleam
import gleam/io
import apollo/t
import apollo/filter as f

pub type Post {
  Post(id: String, timestamp: Int, title: String, content: String)
}

pub fn main() {
  let post_schema: t.Schema(Post) =
    apollo.typed(
      Post,
      [
        #(
          "id",
          t.string()
          |> f.min(4)
          |> f.max(10),
        ),
        #(
          "timestamp",
          t.int()
          |> f.length(10),
        ),
        #(
          "title",
          t.string()
          |> f.min(3)
          |> f.max(40),
        ),
        #("content", t.string()),
      ],
    )

  let post =
    apollo.validate(
      post_schema,
      #(
        #("id", "549"),
        // id should be at least 4 characters
        #("timestamp", 1_691_611_376),
        #("title", "Apollo, the God of Truth and Prophecy"),
        #(
          "content",
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
        ),
      ),
    )
  io.debug(post) // Error("Length of \"549\" is less than than 4")
}
```

## Installation

This package can be added to your Gleam project:

```sh
gleam add apollo
```

and its documentation can be found at <https://hexdocs.pm/apollo>.
