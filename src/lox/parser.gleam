import lox/lexer.{type LexError}
import lox/token.{type Token}

pub type ParseError {
  ParseError(message: String, line_number: Int)
}

pub type ParseResult {
  ParseResult(errors: List(LexError))
}

type ParseState {
  ParseState(tokens: List(Token), errors: List(LexError))
}

pub fn parse(tokens: List(Token)) -> ParseResult {
  traverse(ParseState(tokens, []))
}

fn traverse(parse_state: ParseState) -> ParseResult {
  ParseResult(parse_state.errors)
}
