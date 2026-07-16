import gleam/float
import gleam/int
import lox/expr.{type Expr}
import lox/token.{type Token}

pub type ParseError {
  ParseError(message: String, token: Token)
}

pub type ParseResult {
  ParseResult(expr: Result(Expr, Nil), errors: List(ParseError))
}

type ParseState {
  ParseState(tokens: List(Token), errors: List(ParseError))
}

pub fn parse(tokens: List(Token)) -> ParseResult {
  parse_unary(ParseState(tokens, []))
  todo
}

fn parse_expression(state: ParseState) -> #(Expr, ParseState) {
  parse_factor(state)
}

fn parse_factor(state: ParseState) -> #(Expr, ParseState) {
  let #(left, left_state) = parse_unary(state)
  let assert [hd, ..r] = left_state.tokens
  case hd.type_ {
    token.Star | token.Slash -> {
      let #(next_expression, next_state) =
        parse_unary(ParseState(..left_state, tokens: r))
      #(expr.Binary(left, hd, next_expression), next_state)
    }
    _ -> #(left, left_state)
  }
}

fn parse_unary(state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.Bang | token.Minus -> {
      let #(expr, state) = parse_unary(ParseState(..state, tokens: r))
      #(expr.Unary(hd, expr), ParseState(..state, tokens: r))
    }
    _ -> parse_primary(state)
  }
}

fn parse_primary(state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  let state = ParseState(..state, tokens: r)
  case hd.type_ {
    token.Number -> {
      let val = case float.parse(hd.lexeme) {
        Ok(f) -> f
        _ ->
          case int.parse(hd.lexeme) {
            Ok(i) -> int.to_float(i)
            _ -> panic as "Cannot parse number"
          }
      }
      #(expr.Literal(expr.NumberVal(val)), state)
    }
    token.String -> #(expr.Literal(expr.StringVal(hd.lexeme)), state)
    token.False -> #(expr.Literal(expr.BoolVal(False)), state)
    token.True -> #(expr.Literal(expr.BoolVal(True)), state)
    token.Nil -> #(expr.Literal(expr.NilVal), state)
    token.LeftParen -> {
      let #(inner, state) = parse_expression(state)
      let state = case state.tokens {
        [close, ..rest] if close.type_ == token.RightParen ->
          ParseState(..state, tokens: rest)
        _ -> {
          let err = ParseError("Expected ')' after expression.", hd)
          ParseState(..state, errors: [err, ..state.errors])
        }
      }
      #(expr.Grouping(inner), state)
    }
    _ -> panic as "empty token list - lexer must emit EOF"
  }
}
