import gleam/dict
import gleam/io
import gleam/list
import gleam/string
import lox/constants
import lox/token.{type Token, Token}
import lox/utils

pub fn scan(source: String) -> Nil {
  let tokens = tokenize(LexState(source, [], 1, []))
  io.println(string.inspect(tokens))
}

fn scan_comment(lex_state: LexState) -> LexState {
  case lex_state.source {
    "\n" <> _ -> lex_state
    _ -> {
      case string.pop_grapheme(lex_state.source) {
        Ok(#(_, rest)) -> scan_comment(LexState(..lex_state, source: rest))
        Error(_) -> lex_state
      }
    }
  }
}

fn scan_string_literal(lex_state: LexState, literal: String) -> LexState {
  let LexState(source:, tokens:, line_number:, errors:) = lex_state
  case source {
    "\n" as c <> r ->
      scan_string_literal(
        LexState(..lex_state, source: r, line_number: line_number + 1),
        c <> literal,
      )
    "\"" <> r -> {
      let string_literal = string.reverse(literal)
      let token =
        Token(token.String, string_literal, string_literal, line_number)
      LexState(..lex_state, source: r, tokens: [token, ..tokens])
    }
    _ -> {
      case string.pop_grapheme(source) {
        Ok(#(char, r)) ->
          scan_string_literal(LexState(..lex_state, source: r), char <> literal)
        Error(_) -> {
          let error = LexError("Unterminated string literal", line_number)
          LexState(..lex_state, errors: [error, ..errors])
        }
      }
    }
  }
}

fn scan_keyword_or_identifier(
  lex_state: LexState,
  literal: String,
) -> LexState {
  let LexState(source:, tokens:, line_number:, errors:) = lex_state
  let finish = fn(r) {
    let word = string.reverse(literal)
    let keyword_map = constants.get_keyword_map()
    let keyword = dict.get(keyword_map, word)
    let token = case keyword {
      Ok(keyword) -> Token(keyword, word, word, line_number)
      _ -> Token(token.Identifier, word, word, line_number)
    }
    LexState(r, [token, ..tokens], line_number, errors)
  }
  case source {
    _ -> {
      case string.pop_grapheme(source) {
        Ok(#(char, r)) -> {
          let is_alphanumeric = utils.is_alphanumeric(char)
          case is_alphanumeric {
            True ->
              scan_keyword_or_identifier(
                LexState(r, tokens, line_number, errors),
                char <> literal,
              )
            False -> finish(source)
          }
        }
        Error(_) -> finish("")
      }
    }
  }
}

fn scan_number_literal(
  lex_state: LexState,
  literal: String,
  contains_period: Bool,
) -> LexState {
  let LexState(source:, tokens:, line_number:, errors:) = lex_state
  let finish = fn() {
    let token = Token(token.Number, literal, literal, line_number)
    LexState(source, [token, ..tokens], line_number, errors)
  }
  case source, contains_period {
    "." as c <> r, False ->
      case string.pop_grapheme(r) {
        Ok(#(hd, r)) -> {
          let is_number = utils.is_number(hd)
          case is_number {
            True ->
              scan_number_literal(
                LexState(..lex_state, source: r),
                hd <> c <> literal,
                True,
              )
            _ -> {
              finish()
            }
          }
        }
        _ -> finish()
      }
    _, _ -> {
      case string.pop_grapheme(source) {
        Ok(#(hd, r)) -> {
          let is_number = utils.is_number(hd)
          case is_number {
            True ->
              scan_number_literal(
                LexState(r, tokens, line_number, errors),
                hd <> literal,
                contains_period,
              )
            _ -> {
              finish()
            }
          }
        }
        _ -> finish()
      }
    }
  }
}

pub type LexError {
  LexError(message: String, line_number: Int)
}

pub type LexResult {
  LexResult(tokens: List(Token), errors: List(LexError))
}

pub type LexState {
  LexState(
    source: String,
    tokens: List(Token),
    line_number: Int,
    errors: List(LexError),
  )
}

fn tokenize(lex_state: LexState) -> LexResult {
  let LexState(source:, tokens:, line_number:, errors:) = lex_state
  let make_token = fn(tt, lex) { Token(tt, lex, "", line_number) }
  let make_token_and_continue = fn(tt, lex, remaining) {
    let t = make_token(tt, lex)
    tokenize(LexState(remaining, [t, ..tokens], line_number, errors))
  }
  let new_line = fn(rest: String) {
    tokenize(LexState(rest, tokens, line_number + 1, errors))
  }
  let skip_char = fn(rest: String) {
    tokenize(LexState(rest, tokens, line_number, errors))
  }
  case source {
    // EOF of file
    "" -> {
      let eof = Token(token.Eof, "", "", line_number)
      LexResult(list.reverse([eof, ..tokens]), [])
    }
    "!=" as c <> rest -> make_token_and_continue(token.BangEqual, c, rest)
    "==" as c <> rest -> make_token_and_continue(token.EqualEqual, c, rest)
    "<=" as c <> rest -> make_token_and_continue(token.LessEqual, c, rest)
    ">=" as c <> rest -> make_token_and_continue(token.GreaterEqual, c, rest)
    "//" <> rest ->
      tokenize(scan_comment(LexState(rest, tokens, line_number, errors)))
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
    "\"" <> rest ->
      tokenize(scan_string_literal(
        LexState(rest, tokens, line_number, errors),
        "",
      ))
    _ -> {
      case string.pop_grapheme(source) {
        Ok(#(hd, r)) -> {
          let is_number = utils.is_number(hd)
          let is_letter = utils.is_letter(hd)
          case is_number, is_letter {
            True, _ -> {
              tokenize(scan_number_literal(
                LexState(r, tokens, line_number, errors),
                hd,
                False,
              ))
            }
            _, True -> {
              tokenize(scan_keyword_or_identifier(
                LexState(r, tokens, line_number, errors),
                hd,
              ))
            }
            _, _ -> {
              io.println(hd)
              panic as "idk1"
            }
          }
        }
        Error(_) -> panic as "idk2"
      }
    }
  }
}
