import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import lox/lexer
import simplifile

pub fn main() -> Nil {
  case argv.load().arguments {
    [path] -> run_file(path)
    [] -> {
      io.println("No argument provided")
    }
    _ -> Nil
  }
}

fn run_file(path: String) -> Nil {
  case simplifile.read(path) {
    Ok(source) -> run(source)
    Error(error) -> {
      io.println(
        "Failed to read " <> path <> ": " <> simplifile.describe_error(error),
      )
    }
  }
}

fn run(source: String) -> Nil {
  let result = lexer.scan(source)
  case result.errors {
    [] -> {
      // TODO: hand result.tokens to the parser when it exists
      io.println(string.inspect(result.tokens))
    }
    errors -> report_errors(errors)
  }
}

fn report_errors(errors: List(lexer.LexError)) -> Nil {
  io.println(string.inspect(list.length(errors)) <> " Lox Error(s)")
  errors
  |> list.reverse
  |> list.each(fn(e) {
    io.println_error(
      "[line " <> int.to_string(e.line_number) <> "] Error: " <> e.message,
    )
  })
}
