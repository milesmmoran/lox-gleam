pub type TokenType {
  // Single-character tokens
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  Comma
  Dot
  Minus
  Plus
  Semicolon
  Slash
  Star

  // One or two character tokens
  Bang
  BangEqual
  Equal
  EqualEqual
  Greater
  GreaterEqual
  Less
  LessEqual

  // Literals
  Identifier
  String
  Number

  // Keywords
  And
  Class
  Else
  False
  Fun
  For
  If
  Nil
  Or
  Print
  Return
  Super
  This
  True
  Var
  While

  // Special
  Eof
}

pub type Token {
  Token(type_: TokenType, lexeme: String, line: Int)
}

pub fn classify(word: String) -> TokenType {
  case word {
    "and" -> And
    "class" -> Class
    "else" -> Else
    "false" -> False
    "fun" -> Fun
    "for" -> For
    "if" -> If
    "nil" -> Nil
    "or" -> Or
    "print" -> Print
    "return" -> Return
    "super" -> Super
    "this" -> This
    "true" -> True
    "var" -> Var
    "while" -> While
    _ -> Identifier
  }
}
