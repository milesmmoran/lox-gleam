import gleam/option.{type Option}
import lox/token.{type Token}

pub type Declaration {
  VarDecl(var: String, expr: Option(Expr))
  Statement(statement: Statement)
}

pub type Statement {
  ExprStmt(expr: Expr)
  PrintStmt(expr: Expr)
}

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
