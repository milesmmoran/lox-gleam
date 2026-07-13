import gleam/io
import gleam/list
import gleam/string

import lox/token.{type Token, Token}

pub fn scan(source: String) -> Nil {
  let chars = string.to_graphemes(source)
  let tokens = scan_(chars, [], 1)
  io.println(string.inspect(tokens))
}

fn scan_(chars: List(String), tokens: List(Token), i: Int) -> List(Token) {
  let make_token = fn(tt, lex) { Token(tt, lex, "", i) }
  case chars {
    // EOF of file
    [] -> {
      let eof = Token(token.Eof, "", "", i)
      list.reverse([eof, ..tokens])
    }
    // Single Char 
    [hd, ..rest] as c -> {
      let make_token_and_continue = fn(tt) {
        let t = make_token(tt, hd)
        scan_(rest, [t, ..tokens], i)
      }
      case c {
        ["\n", ..] -> {
          scan_(rest, tokens, i + 1)
        }
        ["(", ..] -> {
          make_token_and_continue(token.LeftParen)
        }
        [")", ..] -> {
          make_token_and_continue(token.RightParen)
        }
        ["{", ..] -> {
          make_token_and_continue(token.LeftBrace)
        }
        ["}", ..] -> {
          make_token_and_continue(token.LeftBrace)
        }
        [",", ..] -> {
          make_token_and_continue(token.Comma)
        }
        [".", ..] -> {
          make_token_and_continue(token.Dot)
        }
        ["-", ..] -> {
          make_token_and_continue(token.Minus)
        }
        ["+", ..] -> {
          make_token_and_continue(token.Plus)
        }
        [";", ..] -> {
          make_token_and_continue(token.Semicolon)
        }
        ["*", ..] -> {
          make_token_and_continue(token.Star)
        }
        [hd, ..] -> {
          io.println(
            "Encountered unknown character: '"
            <> hd
            <> "' "
            <> "on line "
            <> string.inspect(i),
          )
          panic
        }
        [] -> panic as "unreachable"
      }
    }
  }
}
