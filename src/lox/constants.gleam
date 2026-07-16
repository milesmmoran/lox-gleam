import lox/token

pub fn classify(word: String) -> token.TokenType {
  case word {
    "and" -> token.And
    "class" -> token.Class
    "else" -> token.Else
    "false" -> token.False
    "fun" -> token.Fun
    "for" -> token.For
    "if" -> token.If
    "nil" -> token.Nil
    "or" -> token.Or
    "print" -> token.Print
    "return" -> token.Return
    "super" -> token.Super
    "this" -> token.This
    "true" -> token.True
    "var" -> token.Var
    "while" -> token.While
    _ -> token.Identifier
  }
}
