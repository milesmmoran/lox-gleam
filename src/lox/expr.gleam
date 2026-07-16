import lox/token.{type Token}

pub type Expr {
  Binary(left: Expr, op: Token, right: Expr)
  Unary(op: Token, operand: Expr)
  Literal(value: LiteralValue)
  Grouping(expr: Expr)
}

pub type LiteralValue {
  NumberVal(Float)
  StringVal(String)
  BoolVal(Bool)
  NilVal
}
