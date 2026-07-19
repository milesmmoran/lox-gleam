import gleam/dict
import gleam/option.{type Option}
import lox/token.{type Token}

pub type Declaration {
  VarDecl(var: String, expr: Option(Expr))
  FunDecl(name: String, params: List(String), body: Declaration)
  Statement(statement: Statement)
}

pub type Statement {
  ExprStmt(expr: Expr)
  PrintStmt(expr: Expr)
  BlockStmt(declarations: List(Declaration))
  IfStmt(cond: Expr, then_branch: Declaration, else_branch: Option(Declaration))
  WhileStmt(cond: Expr, body: Declaration)
  ForStmt(
    init: Option(Declaration),
    cond: Option(Expr),
    incr: Option(Expr),
    body: Declaration,
  )
}

pub type Expr {
  Binary(left: Expr, op: Token, right: Expr)
  Logical(left: Expr, op: Token, right: Expr)
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
  FunVal(params: List(String), body: Declaration, env: Env)
}

pub type Scope =
  dict.Dict(String, LiteralValue)

pub type Env {
  Env(scopes: List(Scope))
}
