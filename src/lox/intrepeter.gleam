import gleam/float
import lox/expr.{type Expr}
import lox/token

pub fn eval(expr: Expr) -> expr.LiteralValue {
  case expr {
    expr.Literal(val) -> val
    expr.Grouping(e) -> eval(e)
    expr.Unary(operand, expr) -> {
      let val = eval(expr)
      case operand.type_ {
        token.Minus ->
          case val {
            expr.NumberVal(n) -> expr.NumberVal(float.negate(n))
            _ -> panic
          }
        token.Bang -> {
          case val {
            expr.BoolVal(b) -> expr.BoolVal(!b)
            _ -> panic
          }
        }
        _ -> panic
      }
    }
    expr.Binary(left, op, right) -> {
      let left_val = eval(left)
      let right_val = eval(right)
      case op.type_ {
        token.Plus ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.NumberVal(l +. r)
            expr.StringVal(l), expr.StringVal(r) -> expr.StringVal(l <> r)
            _, _ -> panic
          }
        token.Minus ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.NumberVal(l -. r)
            _, _ -> panic
          }
        token.Slash ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.NumberVal(l /. r)
            _, _ -> panic
          }

        token.Star ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.NumberVal(l *. r)
            _, _ -> panic
          }
        token.Less ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.BoolVal(l <. r)
            _, _ -> panic
          }

        token.LessEqual ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.BoolVal(l <=. r)
            _, _ -> panic
          }

        token.Greater ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.BoolVal(l >. r)
            _, _ -> panic
          }

        token.GreaterEqual ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.BoolVal(l >=. r)
            _, _ -> panic
          }

        token.EqualEqual ->
          case left_val, right_val {
            expr.NumberVal(_), expr.NumberVal(_) ->
              expr.BoolVal(is_truthy(left_val) == is_truthy(right_val))
            expr.BoolVal(_), expr.BoolVal(_) ->
              expr.BoolVal(is_truthy(left_val) == is_truthy(right_val))
            _, _ -> panic
          }

        token.BangEqual ->
          case left_val, right_val {
            expr.NumberVal(_), expr.NumberVal(_) ->
              expr.BoolVal(is_truthy(left_val) != is_truthy(right_val))
            expr.BoolVal(_), expr.BoolVal(_) ->
              expr.BoolVal(is_truthy(left_val) != is_truthy(right_val))
            _, _ -> panic
          }

        _ -> panic
      }
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
