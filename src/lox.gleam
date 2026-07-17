import argv
import gleam/io
import lox/interpreter
import lox/lexer
import lox/parser
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
  let scan_res = lexer.scan(source)
  let parse_res = parser.parse(scan_res.tokens)
  case parse_res.expr {
    Ok(declarations) -> interpreter.eval(declarations)
    _ -> panic
  }
}
