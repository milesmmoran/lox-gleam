import argv
import gleam/io
import lox/format
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
  source
  |> lexer.scan
  |> format.format_result
  |> io.print
}
