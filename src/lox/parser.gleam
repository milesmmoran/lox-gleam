import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import lox/error.{type ParseError, ParseError}
import lox/expr.{type Declaration, type Expr}
import lox/token.{type Token}

pub type ParseResult {
  ParseResult(program: Result(List(Declaration), Nil), errors: List(ParseError))
}

type ParseState {
  ParseState(tokens: List(Token), errors: List(ParseError))
}

pub fn parse(tokens: List(Token)) -> ParseResult {
  let #(declarations, state) = parse_declarations(ParseState(tokens, []), [])
  ParseResult(Ok(declarations), list.reverse(state.errors))
}

fn consume(
  state: ParseState,
  expected: token.TokenType,
  message: String,
) -> ParseState {
  case state.tokens {
    [hd, ..rest] if hd.type_ == expected -> ParseState(..state, tokens: rest)
    [hd, ..] -> {
      let err = ParseError(message, hd)
      ParseState(..state, errors: [err, ..state.errors])
    }
    [] -> state
  }
}

fn peek(state: ParseState) -> Token {
  let assert [hd, ..] = state.tokens
  hd
}

fn advance(state: ParseState) -> #(Token, ParseState) {
  let assert [hd, ..rest] = state.tokens
  #(hd, ParseState(..state, tokens: rest))
}

fn parse_declarations(
  state: ParseState,
  declarations: List(Declaration),
) -> #(List(Declaration), ParseState) {
  case peek(state).type_ {
    token.Eof -> #(list.reverse(declarations), state)
    _ -> {
      let #(decl, state) = parse_declaration(state)
      parse_declarations(state, [decl, ..declarations])
    }
  }
}

fn parse_declaration(state: ParseState) -> #(Declaration, ParseState) {
  let #(hd1, state1) = advance(state)
  case hd1.type_ {
    token.Var -> {
      let #(hd2, state2) = advance(state1)
      case hd2.type_ {
        token.Identifier -> {
          let name = hd2.lexeme
          let #(hd3, state3) = advance(state2)
          case hd3.type_ {
            token.Equal -> {
              let #(init, state4) = parse_expression(state3)
              let state5 =
                consume(state4, token.Semicolon, "Expected ';' after value.")
              let decl = expr.VarDecl(name, option.Some(init))
              #(decl, state5)
            }
            token.Semicolon -> {
              let decl = expr.VarDecl(name, option.None)
              #(decl, state3)
            }
            _ -> panic
          }
        }
        _ -> panic as "you need to finish your variable decl, no identifier"
      }
    }
    token.Fun -> {
      // consume FUN
      let #(iden, state2) = advance(state1)
      case iden.type_ {
        token.Identifier -> {
          let state3 = consume(state2, token.LeftParen, "Expecting opening (")
          let #(args, state4) = parse_fun_arg_loop(state3, [])
          let #(body, state5) = parse_declaration(state4)
          let func = expr.FunDecl(iden.lexeme, args, body)
          #(func, state5)
        }
        _ -> panic as "expected identifier"
      }
    }
    token.LeftBrace -> {
      let #(decls, state2) = parse_block_statement_loop(state1, [])
      let st = expr.BlockStmt(decls)
      let decl = expr.Statement(st)
      #(decl, state2)
    }
    token.Print -> {
      let #(expr, state) = parse_expression(state1)
      let state =
        consume(state, token.Semicolon, "Expected ';' after expression.")
      let statement = expr.PrintStmt(expr)
      let decl = expr.Statement(statement)
      #(decl, state)
    }
    token.If -> {
      let #(cond, post_cond_state) = parse_expression(state1)
      let #(then, post_then_state) = parse_declaration(post_cond_state)
      case post_then_state.tokens {
        [hd, ..r] if hd.type_ == token.Else -> {
          let #(else_, post_else_state) =
            parse_declaration(ParseState(..post_then_state, tokens: r))
          let if_decl = expr.IfStmt(cond, then, Some(else_))
          let s = expr.Statement(if_decl)
          #(s, post_else_state)
        }
        _ -> {
          let if_decl = expr.IfStmt(cond, then, None)
          let s = expr.Statement(if_decl)
          #(s, post_then_state)
        }
      }
    }
    token.For -> {
      let post_paren_state =
        consume(state1, token.LeftParen, "Expected '(' after for.")
      let init_peek = peek(post_paren_state)
      // init
      let #(init, post_init_state) = case init_peek.type_ {
        token.Semicolon -> #(
          None,
          consume(post_paren_state, token.Semicolon, ""),
        )
        _ -> {
          let #(decl, post_init_state) = parse_declaration(post_paren_state)
          #(Some(decl), post_init_state)
        }
      }
      // cond
      let cond_peek = peek(post_init_state)
      let #(cond, post_cond_state) = case cond_peek.type_ {
        token.Semicolon -> #(
          None,
          consume(post_init_state, token.Semicolon, ""),
        )
        _ -> {
          let #(cond, post_cond_expr_state) = parse_expression(post_init_state)
          let post_cond_state =
            consume(post_cond_expr_state, token.Semicolon, "")
          #(Some(cond), post_cond_state)
        }
      }
      // incr
      let incr_peek = peek(post_cond_state)
      let #(incr, post_incr_state) = case incr_peek.type_ {
        token.RightParen -> #(
          None,
          consume(post_cond_state, token.RightParen, ""),
        )
        _ -> {
          let #(incr, post_incr_expr_state) = parse_expression(post_cond_state)
          let post_incr_state =
            consume(post_incr_expr_state, token.RightParen, "")
          #(Some(incr), post_incr_state)
        }
      }
      let #(then, post_for_state) = parse_declaration(post_incr_state)
      let for_declr = expr.ForStmt(init, cond, incr, then)
      let s = expr.Statement(for_declr)
      #(s, post_for_state)
    }
    token.While -> {
      let #(cond, post_cond_state) = parse_expression(state1)
      let #(then, post_while_state) = parse_declaration(post_cond_state)
      let while_declr = expr.WhileStmt(cond, then)
      let s = expr.Statement(while_declr)
      #(s, post_while_state)
    }
    _ -> {
      let #(expr, new_state) = parse_expression(state)
      let state =
        consume(new_state, token.Semicolon, "Expected ';' after expression.")
      let statement = expr.ExprStmt(expr)
      let decl = expr.Statement(statement)
      #(decl, state)
    }
  }
}

fn parse_fun_arg_loop(
  state: ParseState,
  args: List(String),
) -> #(List(String), ParseState) {
  let hd = peek(state)
  case hd.type_ {
    token.Eof -> panic as "unterminated"
    token.RightParen -> {
      let state2 = consume(state, token.RightParen, "")
      #(list.reverse(args), state2)
    }
    token.Identifier -> {
      let #(hd1, state2) = advance(state)
      let hd2 = peek(state2)
      case hd2.type_ {
        token.Eof -> panic as "unterminated"
        token.Comma -> {
          let state3 = consume(state2, token.Comma, "")
          parse_fun_arg_loop(state3, [hd1.lexeme, ..args])
        }
        token.RightParen -> {
          parse_fun_arg_loop(state2, [hd1.lexeme, ..args])
        }
        _ -> panic
      }
    }
    _ -> panic as "unexpected token during fun initiall"
  }
}

fn parse_block_statement_loop(
  state: ParseState,
  decls: List(Declaration),
) -> #(List(Declaration), ParseState) {
  let hd = peek(state)
  case hd.type_ {
    token.Eof -> panic as "unterminated block"
    token.RightBrace -> {
      let state2 = consume(state, token.RightBrace, "")
      #(list.reverse(decls), state2)
    }
    _ -> {
      let #(decl, state2) = parse_declaration(state)
      parse_block_statement_loop(state2, [decl, ..decls])
    }
  }
}

fn parse_expression(state: ParseState) -> #(Expr, ParseState) {
  parse_assignment(state)
}

fn parse_assignment(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_or(state)
  case peek(state).type_ {
    token.Equal -> {
      let #(_, state1) = advance(state)
      let #(expr, state2) = parse_or(state1)
      case left {
        expr.Identifier(name) -> #(expr.Assignment(name, expr), state2)
        _ -> panic
      }
    }
    _ -> #(left, state)
  }
}

fn parse_or(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_and(state)
  parse_or_loop(left, state)
}

fn parse_or_loop(left: Expr, state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.Or -> {
      let #(right, state) = parse_and(ParseState(..state, tokens: r))
      parse_or_loop(expr.Logical(left, hd, right), state)
    }
    _ -> #(left, state)
  }
}

fn parse_and(state: ParseState) -> #(Expr, ParseState) {
  let #(left, state) = parse_equality(state)
  parse_and_loop(left, state)
}

fn parse_and_loop(left: Expr, state: ParseState) -> #(Expr, ParseState) {
  let assert [hd, ..r] = state.tokens
  case hd.type_ {
    token.And -> {
      let #(right, state) = parse_equality(ParseState(..state, tokens: r))
      parse_and_loop(expr.Logical(left, hd, right), state)
    }
    _ -> #(left, state)
  }
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
    token.Identifier -> #(expr.Identifier(hd.lexeme), state)
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
