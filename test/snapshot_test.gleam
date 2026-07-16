import gleam/int
import gleam/list
import gleam/string
import lox/format
import lox/lexer
import simplifile

const programs_dir = "test/programs"

pub fn snapshot_test() {
  let assert Ok(entries) = simplifile.read_directory(programs_dir)
  let sources =
    entries
    |> list.filter(string.ends_with(_, ".lox"))
    |> list.sort(string.compare)

  let failures =
    sources
    |> list.filter_map(check_one)

  case failures {
    [] -> Nil
    _ -> {
      let msg =
        "Snapshot mismatch in "
        <> int.to_string(list.length(failures))
        <> " file(s):\n"
        <> string.join(failures, "\n\n")
      panic as msg
    }
  }
}

fn check_one(name: String) -> Result(String, Nil) {
  let source_path = programs_dir <> "/" <> name
  let expected_path = source_path <> ".expected"
  let assert Ok(source) = simplifile.read(source_path)
  let actual = source |> lexer.scan |> format.format_result

  case simplifile.read(expected_path) {
    // Soft-bless: no snapshot yet → write it and pass.
    Error(_) -> {
      let assert Ok(_) = simplifile.write(expected_path, actual)
      Error(Nil)
    }
    Ok(expected) -> {
      case expected == actual {
        True -> Error(Nil)
        False -> Ok(diff_message(name, expected, actual))
      }
    }
  }
}

fn diff_message(name: String, expected: String, actual: String) -> String {
  "--- "
  <> name
  <> " ---\nEXPECTED:\n"
  <> expected
  <> "\nACTUAL:\n"
  <> actual
}
