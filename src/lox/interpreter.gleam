import gleam/dict
import gleam/float
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import lox/expr.{type Declaration, type Env, type Expr, Env}
import lox/token

pub fn eval(decls: List(Declaration)) -> Nil {
  let env = Env([dict.new()], dict.new(), 0)
  let _ = eval_statements(decls, env)
  Nil
}

fn eval_statements(
  statements: List(Declaration),
  env: Env,
) -> #(option.Option(expr.LiteralValue), Env) {
  case statements {
    [] -> #(None, env)
    [hd, ..rest] -> {
      case eval_statement(hd, env) {
        #(Some(value), new_env) -> #(Some(value), new_env)
        #(None, new_env) -> eval_statements(rest, new_env)
      }
    }
  }
}

fn add_var(env: Env, name: String, value: expr.LiteralValue) -> Env {
  let assert [hd, ..r] = env.scopes
  let id = env.next_id
  let hd2 = dict.insert(hd, name, id)
  let store = dict.insert(env.store, id, value)
  Env([hd2, ..r], store, id + 1)
}

fn add_scope(env: Env) -> Env {
  Env(..env, scopes: [dict.new(), ..env.scopes])
}

fn pop_scope(env: Env) -> Env {
  case env.scopes {
    [_, ..rest] -> Env(..env, scopes: rest)
    [] -> env
  }
}

fn update_var(env: Env, name: String, value: expr.LiteralValue) -> Env {
  case get_var_id(env, name) {
    Some(id) -> Env(..env, store: dict.insert(env.store, id, value))
    _ -> panic as "missing from scope"
  }
}

fn get_var_id(env: Env, name: String) -> Option(Int) {
  case env.scopes {
    [] -> None
    [hd, ..r] -> {
      case dict.get(hd, name) {
        Ok(id) -> Some(id)
        _ -> get_var_id(Env(..env, scopes: r), name)
      }
    }
  }
}

fn get_var(env: Env, name: String) -> expr.LiteralValue {
  case get_var_id(env, name) {
    Some(id) -> {
      case dict.get(env.store, id) {
        Ok(val) -> val
        _ -> panic as "missing from store"
      }
    }
    _ -> panic as "missing from scope"
  }
}

fn eval_statement(
  statement: Declaration,
  env: Env,
) -> #(option.Option(expr.LiteralValue), Env) {
  case statement {
    expr.VarDecl(name, option.Some(e)) -> {
      let #(v, env) = eval_expr(e, env)
      #(None, add_var(env, name, v))
    }
    expr.VarDecl(name, option.None) -> #(None, add_var(env, name, expr.NilVal))
    expr.ClassDecl(name, methods) -> {
      let method_dict =
        list.fold(methods, dict.new(), fn(acc, m) {
          case m {
            expr.FunDecl(mname, _, _) -> dict.insert(acc, mname, m)
            _ -> acc
          }
        })
      let cl = expr.ClassVal(name, method_dict)
      #(None, add_var(env, name, cl))
    }
    expr.FunDecl(name, params, body) -> {
      let e = add_var(env, name, expr.NilVal)
      // reserve slot with placeholder
      let fn_val = expr.FunVal(name, params, body, e)

      // closure sees name → slot
      let ee = update_var(e, name, fn_val)
      #(None, ee)
    }
    expr.Statement(expr.ExprStmt(e)) -> {
      let #(_, env) = eval_expr(e, env)
      #(None, env)
    }
    expr.Statement(expr.ReturnStmt(e)) -> {
      // return
      let #(v, env) = eval_expr(e, env)
      #(Some(v), env)
    }
    expr.Statement(expr.PrintStmt(e)) -> {
      let #(v, env) = eval_expr(e, env)
      io.println(stringify(v))
      #(None, env)
    }
    expr.Statement(expr.BlockStmt(decls)) -> {
      let new_env = add_scope(env)
      let #(v, post_env) = eval_statements(decls, new_env)
      #(v, pop_scope(post_env))
    }
    expr.Statement(expr.IfStmt(cond, then_branch, else_branch)) -> {
      let #(cond_val, env) = eval_expr(cond, env)
      case is_truthy(cond_val), else_branch {
        True, _ -> {
          let #(v, e) = eval_statement(then_branch, env)
          #(v, e)
        }
        False, option.Some(else_stmt) -> {
          let #(v, e) = eval_statement(else_stmt, env)
          #(v, e)
        }
        False, option.None -> #(None, env)
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

fn while_loop(
  cond: expr.Expr,
  then: Declaration,
  env: Env,
) -> #(option.Option(expr.LiteralValue), Env) {
  let #(cond_val, env2) = eval_expr(cond, env)
  case is_truthy(cond_val) {
    True -> {
      let #(val, env3) = eval_statement(then, env2)
      case val {
        Some(_) -> #(val, env3)
        _ -> while_loop(cond, then, env3)
      }
    }
    False -> #(None, env2)
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
    expr.Call(callee, _, args) -> {
      let #(func, new_env) = eval_expr(callee, env)
      case func {
        expr.FunVal(_, params, body, closure) -> {
          let #(evaled_args, callee_env) = eval_args(args, new_env, [])
          // Use closure's scopes but the current (caller's) store.
          let call_env =
            Env(
              scopes: closure.scopes,
              store: callee_env.store,
              next_id: callee_env.next_id,
            )
          let c = bind_closure(evaled_args, params, call_env)
          let #(val, post_body_env) = eval_statement(body, c)
          let v = case val {
            None -> expr.NilVal
            Some(v) -> v
          }
          // Propagate store updates back to the caller.
          #(
            v,
            Env(
              scopes: callee_env.scopes,
              store: post_body_env.store,
              next_id: post_body_env.next_id,
            ),
          )
        }
        expr.ClassVal(_class, _fields) -> {
          let instance = expr.InstanceVal(func, dict.new())
          #(instance, env)
        }
        _ -> panic as "not callable"
      }
    }
    expr.Get(target, name) -> {
      let #(evaled, e) = eval_expr(target, env)
      case evaled {
        expr.InstanceVal(_, fields) -> {
          let v = case dict.get(fields, name) {
            Ok(e) -> e
            _ -> panic
          }
          #(v, e)
        }
        _ -> panic
      }
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
    expr.FunVal(name, _, _, _) -> "Function '" <> name <> "'"
    expr.ClassVal(_, _) -> "Instance '"
    expr.InstanceVal(class, _) ->
      case class {
        expr.ClassVal(name, _) -> "<instance " <> name <> ">"
        _ -> "<instance>"
      }
  }
}

fn is_truthy(v: expr.LiteralValue) -> Bool {
  case v {
    expr.NilVal -> False
    expr.BoolVal(False) -> False
    _ -> True
  }
}

fn eval_args(
  args: List(expr.Expr),
  env: Env,
  acc: List(expr.LiteralValue),
) -> #(List(expr.LiteralValue), Env) {
  case args {
    [hd, ..r] -> {
      let #(val, e) = eval_expr(hd, env)
      eval_args(r, e, [val, ..acc])
    }
    [] -> #(list.reverse(acc), env)
  }
}

fn bind_closure(
  evaled: List(expr.LiteralValue),
  params: List(String),
  closure: Env,
) -> Env {
  let cc = add_scope(closure)
  bind_closure_loop(evaled, params, cc)
}

fn bind_closure_loop(
  evaled: List(expr.LiteralValue),
  params: List(String),
  closure: Env,
) -> Env {
  case evaled, params {
    [hd, ..r], [hd2, ..r2] -> {
      let new_env = add_var(closure, hd2, hd)
      bind_closure_loop(r, r2, new_env)
    }
    [], [] -> closure
    _, _ -> panic as "arity crazy"
  }
}
