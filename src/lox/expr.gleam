import gleam/dict.{type Dict}
import gleam/option.{type Option}
import lox/token.{type Token}

pub type Declaration {
  VarDecl(var: String, expr: Option(Expr))
  FunDecl(name: String, params: List(String), body: Declaration)
  ClassDecl(name: String, methods: List(Declaration))
  Statement(statement: Statement)
}

pub type Statement {
  ExprStmt(expr: Expr)
  PrintStmt(expr: Expr)
  BlockStmt(declarations: List(Declaration))
  IfStmt(cond: Expr, then_branch: Declaration, else_branch: Option(Declaration))
  WhileStmt(cond: Expr, body: Declaration)
  ReturnStmt(expr: Expr)
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
  Get(target: Expr, name: String)
  Set(target: Expr, name: String, value: Expr)
  Identifier(name: String)
  Assignment(name: String, expr: Expr)
  Call(callee: Expr, paren: Token, arguments: List(Expr))
}

pub type LiteralValue {
  NumberVal(Float)
  StringVal(String)
  BoolVal(Bool)
  NilVal
  FunVal(name: String, params: List(String), body: Declaration, env: Env)
  ClassVal(name: String, methods: Dict(String, Declaration))
  InstanceVal(id: Int)
  InstanceData(class: LiteralValue, fields: Dict(String, LiteralValue))
}

pub type Scope =
  dict.Dict(String, Int)

// ID to Store
pub type Store =
  dict.Dict(Int, LiteralValue)

pub type Env {
  Env(scopes: List(Scope), store: Store, next_id: Int)
}
