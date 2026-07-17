import gleam/float
import gleam/int
import gleam/list
import lox/error.{type ParseError, ParseError}
import lox/expr.{type Expr, type Statement}
import lox/token.{type Token}

pub type ParseResult {
  ParseResult(expr: Result(List(Statement), Nil), errors: List(ParseError))
}

type ParseState {
  ParseState(tokens: List(Token), errors: List(ParseError))
}

pub fn parse(tokens: List(Token)) -> ParseResult {
  let #(statements, state) = parse_statements(ParseState(tokens, []), [])
  ParseResult(Ok(statements), list.reverse(state.errors))
}

fn parse_statements(
  state: ParseState,
  statements: List(Statement),
) -> #(List(Statement), ParseState) {
  let assert [hd, ..] = state.tokens
  case hd.type_ {
    token.Eof -> {
      #(list.reverse(statements), state)
    }
    _ -> {
      let #(expr, new_state) = parse_expression(state)
      let es = expr.ExprStmt(expr)
      parse_statements(new_state, [es, ..statements])
    }
  }
}

fn parse_expression(state: ParseState) -> #(Expr, ParseState) {
  parse_equality(state)
}

fn parse_equality(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_comparison(state)
  parse_equality_loop(left, state)
}

fn parse_equality_loop(left: Expr, state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.EqualEqual | token.BangEqual -> {
      let #(right, state) = parse_comparison(ParseState(..state, tokens: r))
      parse_equality_loop(expr.Binary(left, hd, right), state)
    }
    _ -> #(left, state)
  }
}

fn parse_comparison(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_term(state)
  parse_comparison_loop(left, state)
}

fn parse_comparison_loop(left: Expr, state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.Less | token.Greater | token.LessEqual | token.GreaterEqual -> {
      let #(right, state) = parse_term(ParseState(..state, tokens: r))
      parse_comparison_loop(expr.Binary(left, hd, right), state)
    }
    _ -> #(left, state)
  }
}

fn parse_term(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_factor(state)
  parse_term_loop(left, state)
}

fn parse_term_loop(left: Expr, state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.Plus | token.Minus -> {
      let #(right, state) = parse_factor(ParseState(..state, tokens: r))
      parse_term_loop(expr.Binary(left, hd, right), state)
    }
    _ -> #(left, state)
  }
}

fn parse_factor(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_unary(state)
  parse_factor_loop(left, state)
}

fn parse_factor_loop(left: Expr, state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.Star | token.Slash -> {
      let #(right, state) = parse_unary(ParseState(..state, tokens: r))
      parse_factor_loop(expr.Binary(left, hd, right), state)
    }
    _ -> #(left, state)
  }
}

fn parse_unary(state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.Bang | token.Minus -> {
      let #(operand, state) = parse_unary(ParseState(..state, tokens: r))
      #(expr.Unary(hd, operand), state)
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
