import gleam/io
import gleam/list
import gleam/string
import lox/token.{type Token, Token}
import lox/utils

pub fn scan(source: String) -> Nil {
  let tokens = scan_(source, [], 1)
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

fn skip_line(chars: String) -> String {
  case chars {
    "" -> ""
    "\n" <> rest -> rest
    _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(_, rest)) -> skip_line(rest)
        Error(_) -> ""
      }
    }
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
  chars: String,
  literal: String,
  contains_period: Bool,
) -> #(List(String), List(String)) {
  let finish_lex = fn(r) { #(string.reverse(literal), r) }
  case chars, contains_period {
    "." <> _, True -> panic as "unexpected period"
    "." <> r, False -> lex_number(r, "." <> literal, True)
    _, p -> {
      case string.pop_grapheme(chars) {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
          lex_number(r, [hd, ..literal], p)
        _ -> finish_lex(chars)
      }
    }
    [], _ -> finish_lex([])
  }
}

fn lex_identifier(
  chars: List(String),
  literal: List(String),
  contains_period: Bool,
) -> #(List(String), List(String)) {
  let finish_lex = fn(r) { #(list.reverse(literal), r) }
  case chars, contains_period {
    [hd, ..r], p -> {
      case hd {
        // TODO 
        "0"
        | "1"
        | "2"
        | "3"
        | "4"
        | "5"
        | "6"
        | "7"
        | "8"
        | "9"
        | "a"
        | "b"
        | "c"
        | "d"
        | "e"
        | "f"
        | "g"
        | "h"
        | "i"
        | "j"
        | "k"
        | "l"
        | "m"
        | "n"
        | "o"
        | "p"
        | "q"
        | "r"
        | "s"
        | "t"
        | "u"
        | "v"
        | "w"
        | "x"
        | "y"
        | "z" -> lex_identifier(r, [hd, ..literal], p)
        _ -> finish_lex(chars)
      }
    }
    [], _ -> finish_lex([])
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
    "//" <> rest -> new_line(skip_line(rest))
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
    _ -> {
      case string.pop_grapheme(chars) {
        Ok(#(hd, _)) -> {
          let is_number = utils.is_number(hd)
          let is_letter = utils.is_letter(hd)
          case is_number, is_letter {
            True, _ -> {
              let letter = lex_letter()
            }
            _, True -> {
              let number = lex_number()
            }
            _, _ -> panic
          }
        }
        Error(_) -> panic
      }
      // handle number
      // handle 
    }
  }
}
//
// fn scan_(chars: String, tokens: List(Token), i: Int) -> List(Token) {
//   let make_token = fn(tt, lex) { Token(tt, lex, "", i) }
//   case chars {
//     // EOF of file
//     "" -> {
//       let eof = Token(token.Eof, "", "", i)
//       list.reverse([eof, ..tokens])
//     }
//     "!=" as c <> rest -> make_tokens_and_continue(token.BangEqual, char, r)
//     "" <> rest as c -> {
//       let skip_char = fn() { scan_(rest, tokens, i) }
//       let make_token_and_continue = fn(tt) {
//         let t = make_token(tt, hd)
//         scan_(rest, [t, ..tokens], i)
//       }
//       // TODO: Actual errors
//       let make_tokens_and_continue = fn(tt, lex, remaining) {
//         let t = make_token(tt, lex)
//         scan_(remaining, [t, ..tokens], i)
//       }
//       case c {
//         // Comments
//         ["/", "/", ..r] -> new_line(skip_line(r))
//         // New Line
//         ["\n", ..] -> new_line(rest)
//         // White Space
//         [" ", ..] | ["\t", ..] | ["\r", ..] -> skip_char()
//         // Single Chars
//         ["(", ..] -> make_token_and_continue(token.LeftParen)
//         [")", ..] -> make_token_and_continue(token.RightParen)
//         ["{", ..] -> make_token_and_continue(token.LeftBrace)
//         ["}", ..] -> make_token_and_continue(token.RightBrace)
//         [",", ..] -> make_token_and_continue(token.Comma)
//         [".", ..] -> make_token_and_continue(token.Dot)
//         ["-", ..] -> make_token_and_continue(token.Minus)
//         ["+", ..] -> make_token_and_continue(token.Plus)
//         [";", ..] -> make_token_and_continue(token.Semicolon)
//         ["*", ..] -> make_token_and_continue(token.Star)
//         ["!", ..] -> make_token_and_continue(token.Bang)
//         ["=", ..] -> make_token_and_continue(token.Equal)
//         ["<", ..] -> make_token_and_continue(token.Less)
//         [">", ..] -> make_token_and_continue(token.Greater)
//         ["/", ..] -> make_token_and_continue(token.Slash)
//         // String Literals
//         ["\"", ..] -> {
//           let #(literal, rest) = skip_to_quote(rest, [])
//           make_tokens_and_continue(token.String, string.concat(literal), rest)
//         }
//         [hd, ..r] -> {
//           // Number Literal
//           case hd {
//             "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
//               let #(literal, rest) = lex_number(r, [hd], False)
//               make_tokens_and_continue(
//                 token.Number,
//                 string.concat(literal),
//                 rest,
//               )
//             }
//             // alphabetical first
//             "a"
//             | "b"
//             | "c"
//             | "d"
//             | "e"
//             | "f"
//             | "g"
//             | "h"
//             | "i"
//             | "j"
//             | "k"
//             | "l"
//             | "m"
//             | "n"
//             | "o"
//             | "p"
//             | "q"
//             | "r"
//             | "s"
//             | "t"
//             | "u"
//             | "v"
//             | "w"
//             | "x"
//             | "y"
//             | "z" -> {
//               // Identifiers
//               let #(literal, rest) = lex_identifier(r, [hd], False)
//               make_tokens_and_continue(
//                 token.Identifier,
//                 string.concat(literal),
//                 rest,
//               )
//             }
//             _ -> panic_with_char(hd, i)
//           }
//         }
//         [] -> panic_with_unreachable()
//       }
//     }
//   }
// }
