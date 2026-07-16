import gleam/int
import gleam/list
import gleam/string
import lox/lexer.{type LexError, type LexResult}
import lox/token.{type Token}

pub fn format_result(result: LexResult) -> String {
  let tokens =
    result.tokens
    |> list.map(format_token)
    |> string.join("\n")
  let errors = case result.errors {
    [] -> "(no errors)"
    es ->
      "Errors:\n"
      <> {
        es
        |> list.map(format_error)
        |> string.join("\n")
      }
  }
  tokens <> "\n\n" <> errors <> "\n"
}

fn format_token(t: Token) -> String {
  let type_str = string.inspect(t.type_)
  let padded_type = string.pad_end(type_str, to: 12, with: " ")
  let padded_lexeme =
    string.pad_end(string.inspect(t.lexeme), to: 20, with: " ")
  padded_type <> " " <> padded_lexeme <> " line " <> int.to_string(t.line)
}

fn format_error(e: LexError) -> String {
  "line " <> int.to_string(e.line_number) <> ": " <> e.message
}
