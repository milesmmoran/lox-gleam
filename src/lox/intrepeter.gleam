import gleam/float
import lox/expr.{type Expr}
import lox/token

pub fn eval(expr: Expr) -> expr.LiteralValue {
  case expr {
    expr.Literal(val) -> val
    expr.Grouping(e) -> eval(e)
    expr.Unary(op, expr) -> {
      let val = eval(expr)
      case op.type_ {
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
            expr.NumberVal(l), expr.NumberVal(r) -> expr.BoolVal(l == r)
            expr.BoolVal(l), expr.BoolVal(r) -> expr.BoolVal(l == r)
            _, _ -> panic
          }

        token.BangEqual ->
          case left_val, right_val {
            expr.NumberVal(l), expr.NumberVal(r) -> expr.BoolVal(l != r)
            expr.BoolVal(l), expr.BoolVal(r) -> expr.BoolVal(l != r)
            _, _ -> panic
          }

        _ -> panic
      }
    }
  }
}
