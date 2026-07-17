import lox/token.{type Token}

pub type LexError {
  LexError(message: String, line_number: Int)
}

pub type ParseError {
  ParseError(message: String, token: Token)
}

pub type RuntimeError {
  RuntimeError(message: String, token: Token)
}
