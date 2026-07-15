import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lox/constants
import lox/token.{type Token, Token}
import lox/utils

pub fn scan(source: String) -> Nil {
  let tokens = tokenize(source, [], 1)
  io.println(string.inspect(tokens))
}

fn scan_comment(chars: String) -> String {
  case chars {
    "" -> ""
    "\n" <> rest -> rest
    _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(_, rest)) -> scan_comment(rest)
        Error(_) -> ""
      }
    }
  }
}

fn scan_string_literal(chars: String, literal: String) -> #(String, String) {
  case chars {
    "\"" <> r -> #(string.reverse(literal), r)
    _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(char, r)) -> scan_string_literal(r, char <> literal)
        Error(_) -> panic as "unreachable"
      }
    }
  }
}

fn scan_keyword_or_identifier(
  chars: String,
  literal: String,
) -> #(String, String) {
  let finish_lex = fn(r) { #(string.reverse(literal), r) }
  case chars {
    "\"" <> r -> finish_lex(r)
    // Whitespace variations?
    " " <> r -> finish_lex(r)
    _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(char, r)) -> scan_string_literal(r, char <> literal)
        Error(_) -> panic as "unreachable"
      }
    }
  }
}

fn scan_number_literal(
  chars: String,
  literal: String,
  contains_period: Bool,
) -> #(String, String) {
  let finish_lex = fn(r) { #(string.reverse(literal), r) }
  case chars, contains_period {
    "", _ -> finish_lex("")
    "." <> _, True -> panic as "unexpected period"
    "." <> r, False -> scan_number_literal(r, "." <> literal, True)
    _, _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(hd, r)) -> {
          let is_number = utils.is_number(hd)
          case is_number {
            True -> scan_number_literal(r, hd <> chars, contains_period)
            _ -> finish_lex(r)
          }
        }
        _ -> panic as "unreachable"
      }
    }
  }
}

pub type LexError {
  LexError(message: String, line_number: Int)
}

pub type LexResult {
  LexResult(tokens: List(Token), error_message: Option(LexError))
}

fn tokenize(chars: String, tokens: List(Token), i: Int) -> LexResult {
  let make_token = fn(tt, lex) { Token(tt, lex, "", i) }
  let make_token_and_continue = fn(tt, lex, remaining) {
    let t = make_token(tt, lex)
    tokenize(remaining, [t, ..tokens], i)
  }
  let new_line = fn(rest: String) { tokenize(rest, tokens, i + 1) }
  let skip_char = fn(rest: String) { tokenize(rest, tokens, i) }
  let throw_error = fn(error_message: String) {
    LexResult(list.reverse(tokens), Some(LexError(error_message, i)))
  }
  case chars {
    // EOF of file
    "" -> {
      let eof = Token(token.Eof, "", "", i)
      LexResult(list.reverse([eof, ..tokens]), None)
    }
    "!=" as c <> rest -> make_token_and_continue(token.BangEqual, c, rest)
    "==" as c <> rest -> make_token_and_continue(token.EqualEqual, c, rest)
    "<=" as c <> rest -> make_token_and_continue(token.LessEqual, c, rest)
    ">=" as c <> rest -> make_token_and_continue(token.GreaterEqual, c, rest)
    "//" <> rest -> new_line(scan_comment(rest))
    "\n" <> rest -> new_line(rest)
    // Whitespace
    " " <> rest -> skip_char(rest)
    "\t" <> rest -> skip_char(rest)
    "\r" <> rest -> skip_char(rest)
    //  Single Char
    "(" as c <> rest -> make_token_and_continue(token.LeftParen, c, rest)
    ")" as c <> rest -> make_token_and_continue(token.RightParen, c, rest)
    "{" as c <> rest -> make_token_and_continue(token.LeftBrace, c, rest)
    "}" as c <> rest -> make_token_and_continue(token.RightBrace, c, rest)
    "," as c <> rest -> make_token_and_continue(token.Comma, c, rest)
    "." as c <> rest -> make_token_and_continue(token.Dot, c, rest)
    "+" as c <> rest -> make_token_and_continue(token.Plus, c, rest)
    "-" as c <> rest -> make_token_and_continue(token.Minus, c, rest)
    ";" as c <> rest -> make_token_and_continue(token.Semicolon, c, rest)
    "*" as c <> rest -> make_token_and_continue(token.Star, c, rest)
    "!" as c <> rest -> make_token_and_continue(token.Bang, c, rest)
    "=" as c <> rest -> make_token_and_continue(token.Equal, c, rest)
    "<" as c <> rest -> make_token_and_continue(token.Less, c, rest)
    ">" as c <> rest -> make_token_and_continue(token.Greater, c, rest)
    "/" as c <> rest -> make_token_and_continue(token.Slash, c, rest)
    "\"" <> rest -> {
      let #(string_literal, rest) = scan_string_literal("", rest)
      make_token_and_continue(token.String, string_literal, rest)
    }
    _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(hd, r)) -> {
          let is_number = utils.is_number(hd)
          let is_letter = utils.is_letter(hd)
          case is_number, is_letter {
            True, _ -> {
              let #(number, rest) = scan_number_literal(r, hd, False)
              make_token_and_continue(token.Number, number, rest)
            }
            _, False -> {
              let #(keyword_or_identifier, rest) =
                scan_keyword_or_identifier(r, hd)
              let keyword_map = constants.get_keyword_map()
              let keyword = dict.get(keyword_map, keyword_or_identifier)
              case keyword {
                Ok(keyword) ->
                  make_token_and_continue(keyword, keyword_or_identifier, rest)
                _ ->
                  make_token_and_continue(
                    token.Identifier,
                    keyword_or_identifier,
                    rest,
                  )
              }
              make_token_and_continue(
                token.Identifier,
                keyword_or_identifier,
                rest,
              )
            }
            _, _ -> throw_error("Unknown Error")
          }
        }
        Error(_) -> throw_error("Unkown Error")
      }
    }
  }
}
// So, the issue i'm experiencing now, is error handling doesn't feel great, with these panics. I want to use a clojure, but can't recurse.
