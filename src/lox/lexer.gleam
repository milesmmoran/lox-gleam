import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import lox/token.{type Token, Token}
import lox/utils

pub fn scan(source: String) -> Nil {
  let tokens = scan_(source, [], 1)
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

fn scan_(chars: String, tokens: List(Token), i: Int) -> List(Token) {
  let make_token = fn(tt, lex) { Token(tt, lex, "", i) }
  let make_token_and_continue = fn(tt, lex, remaining) {
    let t = make_token(tt, lex)
    scan_(remaining, [t, ..tokens], i)
  }
  let new_line = fn(rest: String) { scan_(rest, tokens, i + 1) }
  let skip_char = fn(rest: String) { scan_(rest, tokens, i) }
  case chars {
    // EOF of file
    "" -> {
      let eof = Token(token.Eof, "", "", i)
      list.reverse([eof, ..tokens])
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
              // TODO: Move and flesh out keyword map
              let keyword_map = dict.from_list([#("and", token.And)])
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
            _, _ -> panic as "unreachable"
          }
          []
        }
        Error(_) -> panic as "unreachable"
      }
    }
  }
}
