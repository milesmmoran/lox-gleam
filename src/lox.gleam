import argv
import gleam/io
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
  io.println(source)
  let _tokens = lexer.scan(source)
}
