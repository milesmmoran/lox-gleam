import gleam/io
import gleam/list
import gleam/string
import lox/token.{type Token, Token}

pub fn scan(source: String) -> Nil {
  let chars = string.to_graphemes(source)
  let tokens = scan_(chars, [], 1)
  io.println(string.inspect(tokens))
}

fn panic_with_char(char: String, line: Int) -> List(Token) {
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

fn skip_line(chars: List(String)) -> List(String) {
  case chars {
    ["\n", ..r] -> r
    [_, ..r] -> skip_line(r)
    [] -> []
  }
}

fn skip_to_quote(
  chars: List(String),
  literal: List(String),
) -> #(List(String), List(String)) {
  case chars {
    ["\"", ..r] -> #(list.reverse(literal), r)
    [hd, ..r] -> skip_to_quote(r, [hd, ..literal])
    [] -> panic as "unterminated string"
  }
}

fn lex_number(
  chars: List(String),
  literal: List(String),
  contains_period: Bool,
) -> #(List(String), List(String)) {
  let finish_lex = fn(r) { #(list.reverse(literal), r) }
  case chars, contains_period {
    [".", ..], True -> panic as "unexpected period"
    [".", ..r], False -> lex_number(r, [".", ..literal], True)
    //
    [hd, ..r], p -> {
      case hd {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          lex_number(r, [hd, ..literal], p)
        _ -> finish_lex(chars)
      }
    }
    [], _ -> finish_lex([])
  }
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
      let new_line = fn(remaining: List(String)) {
        scan_(remaining, tokens, i + 1)
      }
      let skip_char = fn() { scan_(rest, tokens, i + 1) }
      let make_token_and_continue = fn(tt) {
        let t = make_token(tt, hd)
        scan_(rest, [t, ..tokens], i)
      }
      // TODO: Actual errors
      let make_tokens_and_continue = fn(tt, lex, remaining) {
        let t = make_token(tt, lex)
        scan_(remaining, [t, ..tokens], i)
      }
      case c {
        // Keywords
        ["a", "n", "d", ..r] -> make_tokens_and_continue(token.And, "and", r)
        ["c", "l", "a", "s", "s", ..r] ->
          make_tokens_and_continue(token.Class, "class", r)
        ["e", "l", "s", "e", ..r] ->
          make_tokens_and_continue(token.Else, "else", r)
        ["f", "a", "l", "s", "e", ..r] ->
          make_tokens_and_continue(token.False, "false", r)
        ["f", "o", "r", ..r] -> make_tokens_and_continue(token.For, "for", r)
        ["f", "u", "n", ..r] -> make_tokens_and_continue(token.Fun, "fun", r)
        ["i", "f", ..r] -> make_tokens_and_continue(token.If, "if", r)
        ["n", "i", "l", ..r] -> make_tokens_and_continue(token.Nil, "Nil", r)
        ["o", "r", ..r] -> make_tokens_and_continue(token.Or, "or", r)
        ["p", "r", "i", "n", "t", ..r] ->
          make_tokens_and_continue(token.Print, "print", r)
        ["r", "e", "t", "u", "r", "n", ..r] ->
          make_tokens_and_continue(token.Return, "return", r)
        ["s", "u", "p", "e", "r", ..r] ->
          make_tokens_and_continue(token.Super, "super", r)
        ["t", "h", "i", "s", ..r] ->
          make_tokens_and_continue(token.This, "this", r)
        ["t", "r", "u", "e", ..r] ->
          make_tokens_and_continue(token.True, "true", r)
        ["v", "a", "r", ..r] -> make_tokens_and_continue(token.Var, "var", r)
        ["w", "h", "i", "l", "e", ..r] ->
          make_tokens_and_continue(token.While, "while", r)
        // Ambigious Multi
        ["!", "=", ..r] -> make_tokens_and_continue(token.BangEqual, "!=", r)
        ["=", "=", ..r] -> make_tokens_and_continue(token.EqualEqual, "==", r)
        ["<", "=", ..r] -> make_tokens_and_continue(token.LessEqual, "<=", r)
        [">", "=", ..r] -> make_tokens_and_continue(token.GreaterEqual, ">=", r)
        // Comments
        ["/", "/", ..r] -> new_line(skip_line(r))
        // New Line
        ["\n", ..] -> new_line(rest)
        // White Space
        [" ", ..] | ["\t", ..] | ["\r", ..] -> skip_char()
        // Single Chars
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
        ["!", ..] -> make_token_and_continue(token.Bang)
        ["=", ..] -> make_token_and_continue(token.Equal)
        ["<", ..] -> make_token_and_continue(token.Less)
        [">", ..] -> make_token_and_continue(token.Greater)
        ["/", ..] -> make_token_and_continue(token.Slash)
        // String Literals
        ["\"", ..] -> {
          let #(literal, rest) = skip_to_quote(rest, [])
          make_tokens_and_continue(token.String, string.concat(literal), rest)
        }
        [hd, ..r] -> {
          // Number Literal
          case hd {
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
              io.print("here")
              let #(literal, rest) = lex_number(r, [hd], False)
              make_tokens_and_continue(
                token.Number,
                string.concat(literal),
                rest,
              )
            }
            "a" -> {
              // Identifiers
              todo
            }
            _ -> panic_with_char(hd, i)
          }
        }
        [] -> panic_with_unreachable()
      }
    }
  }
}
