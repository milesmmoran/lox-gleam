import gleam/io
import gleam/list
import gleam/string

import lox/token.{type Token, Token}

pub fn scan(source: String) -> Nil {
  let chars = string.to_graphemes(source)
  let tokens = scan_(chars, [], 1)
  io.println(string.inspect(tokens))
}

fn panic_with_char(char: String, line: int) -> List(Token) {
  io.println(
    "Encountered unknown character: '"
    <> char
    <> "' "
    <> "on line "
    <> string.inspect(line),
  )
  panic
}

fn panic_with_unreachable() -> List(Token) {
  panic as "unreachable"
}

fn scan_(chars: List(String), tokens: List(Token), i: Int) -> List(Token) {
  let make_token = fn(tt, lex) { Token(tt, lex, "", i) }
  case chars {
    // EOF of file
    [] -> {
      let eof = Token(token.Eof, "", "", i)
      list.reverse([eof, ..tokens])
    }
    [hd, ..rest] as c -> {
      let advance = fn(remaining: List(String)) {
        scan_(remaining, tokens, i + 1)
      }
      let make_token_and_continue = fn(tt) {
        let t = make_token(tt, hd)
        scan_(rest, [t, ..tokens], i)
      }
      let make_tokens_and_continue = fn(tt, lex, remaining) {
        let t = make_token(tt, lex)
        scan_(remaining, [t, ..tokens], i)
      }
      case c {
        ["\n", ..] -> advance(rest)
        ["!", "=", ..r] -> make_tokens_and_continue(token.BangEqual, "!=", r)
        ["=", "=", ..r] -> make_tokens_and_continue(token.EqualEqual, "==", r)
        ["<", "=", ..r] -> make_tokens_and_continue(token.LessEqual, "<=", r)
        [">", "=", ..r] -> make_tokens_and_continue(token.GreaterEqual, ">=", r)
        ["/", "/", ..r] -> advance(r)
        ["(", ..] -> make_token_and_continue(token.LeftParen)
        [")", ..] -> make_token_and_continue(token.RightParen)
        ["{", ..] -> make_token_and_continue(token.LeftBrace)
        ["}", ..] -> make_token_and_continue(token.RightBrace)
        [",", ..] -> make_token_and_continue(token.Comma)
        [".", ..] -> make_token_and_continue(token.Dot)
        ["-", ..] -> make_token_and_continue(token.Minus)
        ["+", ..] -> make_token_and_continue(token.Plus)
        [";", ..] -> make_token_and_continue(token.Semicolon)
        ["*", ..] -> make_token_and_continue(token.Star)
        // Possibly Multi
        ["!", ..] -> make_token_and_continue(token.Bang)
        ["=", ..] -> make_token_and_continue(token.Equal)
        ["<", ..] -> make_token_and_continue(token.Less)
        [">", ..] -> make_token_and_continue(token.Greater)
        // this is an example of code smell
        [hd, ..] -> panic_with_char(hd, i)
        [] -> panic_with_unreachable()
      }
    }
  }
}
