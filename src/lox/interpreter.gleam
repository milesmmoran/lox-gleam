import gleam/dict
import gleam/float
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import lox/expr.{type Declaration, type Env, type Expr, type Scope, Env}
import lox/token

pub fn eval(decls: List(Declaration)) -> Nil {
  let env = Env([dict.new()])
  let _ = eval_statements(decls, env)
  Nil
}

fn eval_statements(statements: List(Declaration), env: Env) -> Env {
  case statements {
    [] -> env
    [hd, ..rest] -> {
      let env = eval_statement(hd, env)
      eval_statements(rest, env)
    }
  }
}

fn add_var(env: Env, name: String, value: expr.LiteralValue) -> Env {
  let assert [hd, ..r] = env.scopes
  let hd2 = dict.insert(hd, name, value)
  Env([hd2, ..r])
}

fn add_scope(env: Env) -> Env {
  Env([dict.new(), ..env.scopes])
}

fn pop_scope(env: Env) -> Env {
  case env.scopes {
    [_, ..rest] -> Env(rest)
    [] -> env
  }
}

fn update_var(env: Env, name: String, value: expr.LiteralValue) -> Env {
  Env(update_var_loop(env.scopes, name, value, []))
}

// I didn't write this...
fn update_var_loop(
  remaining: List(Scope),
  name: String,
  value: expr.LiteralValue,
  seen: List(Scope),
) -> List(Scope) {
  case remaining {
    [] -> panic as "Undefined variable."
    [hd, ..r] ->
      case dict.get(hd, name) {
        Ok(_) -> {
          let updated = dict.insert(hd, name, value)
          list.append(list.reverse(seen), [updated, ..r])
        }
        _ -> update_var_loop(r, name, value, [hd, ..seen])
      }
  }
}

fn get_var(env: Env, name: String) -> expr.LiteralValue {
  case env.scopes {
    [] -> panic as "Undefined variable."
    [hd, ..r] -> {
      case dict.get(hd, name) {
        Ok(val) -> val
        _ -> get_var(Env(r), name)
      }
    }
  }
}

fn eval_statement(statement: Declaration, env: Env) -> Env {
  case statement {
    expr.VarDecl(name, option.Some(e)) -> {
      let #(v, env) = eval_expr(e, env)
      add_var(env, name, v)
    }
    expr.VarDecl(name, option.None) -> add_var(env, name, expr.NilVal)
    expr.FunDecl(name, params, body) -> {
      add_var(env, name, expr.FunVal(params, body, env))
    }
    expr.Statement(expr.ExprStmt(e)) -> {
      let #(_, env) = eval_expr(e, env)
      env
    }
    expr.Statement(expr.PrintStmt(e)) -> {
      let #(v, env) = eval_expr(e, env)
      io.println(stringify(v))
      env
    }
    expr.Statement(expr.BlockStmt(decls)) -> {
      let new_env = add_scope(env)
      let post_env = eval_statements(decls, new_env)
      pop_scope(post_env)
    }
    expr.Statement(expr.IfStmt(cond, then_branch, else_branch)) -> {
      let #(cond_val, env) = eval_expr(cond, env)
      case is_truthy(cond_val), else_branch {
        True, _ -> eval_statement(then_branch, env)
        False, option.Some(else_stmt) -> eval_statement(else_stmt, env)
        False, option.None -> env
      }
    }
    expr.Statement(expr.ForStmt(init, cond, incr, then_branch)) -> {
      let cond_val = case cond {
        None -> expr.Literal(expr.BoolVal(True))
        Some(c) -> c
      }

      let then =
        expr.Statement(
          expr.BlockStmt(case incr {
            None -> [then_branch]
            Some(i) -> [then_branch, expr.Statement(expr.ExprStmt(i))]
          }),
        )

      let while = expr.Statement(expr.WhileStmt(cond_val, then))

      case init {
        None -> eval_statement(while, env)
        Some(i) -> {
          let tt = expr.Statement(expr.BlockStmt([i, while]))
          eval_statement(tt, env)
        }
      }
    }
    expr.Statement(expr.WhileStmt(cond, then_branch)) -> {
      while_loop(cond, then_branch, env)
    }
  }
}

fn while_loop(cond: expr.Expr, then: Declaration, env: Env) -> Env {
  let #(cond_val, env2) = eval_expr(cond, env)
  case is_truthy(cond_val) {
    True -> {
      let env3 = eval_statement(then, env2)
      while_loop(cond, then, env3)
    }
    False -> env2
  }
}

fn eval_expr(e: Expr, env: Env) -> #(expr.LiteralValue, Env) {
  case e {
    expr.Literal(val) -> #(val, env)
    expr.Grouping(inner) -> eval_expr(inner, env)
    expr.Identifier(name) -> #(get_var(env, name), env)
    expr.Assignment(name, value_expr) -> {
      let #(v, env) = eval_expr(value_expr, env)
      #(v, update_var(env, name, v))
    }
    expr.Logical(left, op, right) -> {
      case op.type_ {
        token.Or -> {
          let #(l, env) = eval_expr(left, env)
          case is_truthy(l) {
            True -> #(l, env)
            False -> eval_expr(right, env)
          }
        }
        token.And -> {
          let #(l, env) = eval_expr(left, env)
          case !is_truthy(l) {
            True -> #(l, env)
            False -> eval_expr(right, env)
          }
        }
        _ -> panic
      }
    }
    expr.Unary(op, operand) -> {
      let #(val, env) = eval_expr(operand, env)
      case op.type_ {
        token.Minus ->
          case val {
            expr.NumberVal(n) -> #(expr.NumberVal(float.negate(n)), env)
            _ -> panic as "Operand must be a number."
          }
        token.Bang -> #(expr.BoolVal(!is_truthy(val)), env)
        _ -> panic
      }
    }
    expr.Binary(left, op, right) -> {
      let #(l, env) = eval_expr(left, env)
      let #(r, env) = eval_expr(right, env)
      case op.type_ {
        token.Plus ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(
              expr.NumberVal(a +. b),
              env,
            )
            expr.StringVal(a), expr.StringVal(b) -> #(
              expr.StringVal(a <> b),
              env,
            )
            _, _ -> panic as "Operands must be two numbers or two strings."
          }
        token.Minus ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(
              expr.NumberVal(a -. b),
              env,
            )
            _, _ -> panic as "Operands must be numbers."
          }
        token.Slash ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(
              expr.NumberVal(a /. b),
              env,
            )
            _, _ -> panic as "Operands must be numbers."
          }
        token.Star ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(
              expr.NumberVal(a *. b),
              env,
            )
            _, _ -> panic as "Operands must be numbers."
          }
        token.Less ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(expr.BoolVal(a <. b), env)
            _, _ -> panic as "Operands must be numbers."
          }
        token.LessEqual ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(
              expr.BoolVal(a <=. b),
              env,
            )
            _, _ -> panic as "Operands must be numbers."
          }
        token.Greater ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(expr.BoolVal(a >. b), env)
            _, _ -> panic as "Operands must be numbers."
          }
        token.GreaterEqual ->
          case l, r {
            expr.NumberVal(a), expr.NumberVal(b) -> #(
              expr.BoolVal(a >=. b),
              env,
            )
            _, _ -> panic as "Operands must be numbers."
          }
        token.EqualEqual -> #(expr.BoolVal(l == r), env)
        token.BangEqual -> #(expr.BoolVal(l != r), env)
        _ -> panic
      }
    }
  }
}

fn stringify(v: expr.LiteralValue) -> String {
  case v {
    expr.NilVal -> "nil"
    expr.BoolVal(True) -> "true"
    expr.BoolVal(False) -> "false"
    expr.NumberVal(n) -> float.to_string(n)
    expr.StringVal(s) -> s
    expr.FunVal(_, _, _) -> "Function " <> "TBD"
  }
}

fn is_truthy(v: expr.LiteralValue) -> Bool {
  case v {
    expr.NilVal -> False
    expr.BoolVal(False) -> False
    _ -> True
  }
}
