import gleam/option.{type Option}
import lox/token.{type Token}

pub type Declaration {
  VarDecl(var: String, expr: Option(Expr))
  Statement(statement: Statement)
}

pub type Statement {
  ExprStmt(expr: Expr)
  PrintStmt(expr: Expr)
  BlockStmt(declarations: List(Declaration))
  IfStmt(cond: Expr, then_branch: Statement, else_branch: Option(Statement))
}

pub type Expr {
  Binary(left: Expr, op: Token, right: Expr)
  Unary(op: Token, operand: Expr)
  Literal(value: LiteralValue)
  Grouping(expr: Expr)
  Identifier(name: String)
  Assignment(name: String, expr: Expr)
}

pub type LiteralValue {
  NumberVal(Float)
  StringVal(String)
  BoolVal(Bool)
  NilVal
}
