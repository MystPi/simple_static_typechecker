import gleam
import gleam/io
import gleam/dict.{type Dict}
import gleam/string
import gleam/result.{try}

pub type Expr {
  Num(value: Int)
  Str(value: String)
  Bool(value: gleam.Bool)
  Var(name: String)
  BinOp(op: Op, left: Expr, right: Expr)
  If(cond: Expr, then: Expr, otherwise: Expr)
  Fun(name: String, args: Dict(String, Type), return_type: Type, body: Expr)
  Apply(expr: Expr, args: List(Expr))
}

pub type Op {
  Plus
  Minus
  Times
  Less
  Greater
  Concat
}

pub type Type {
  TNum
  TStr
  TBool
  TFun(in: List(Type), out: Type)
}

// This type would probably have more info in a real language, such as the position
// of the error, and possibly other constructors.
pub type TypeError {
  TypeError(message: String)
}

type Scope =
  Dict(String, Type)

/// Find the type of an expression in a given scope.
fn type_of(expr: Expr, scope: Scope) -> Result(Type, TypeError) {
  case expr {
    // To get the type of a variable, all we have to do is look up its name in the
    // scope dictionary. If it doesn't exist, then we return an error
    Var(name) ->
      case dict.get(scope, name) {
        Ok(ty) -> Ok(ty)
        Error(_) -> Error(TypeError("Var not defined: " <> name))
      }

    // Literals are super easy to typecheck. They map 1:1 with their types
    Num(_) -> Ok(TNum)
    Str(_) -> Ok(TStr)
    Bool(_) -> Ok(TBool)

    BinOp(Plus, left, right)
    | BinOp(Minus, left, right)
    | BinOp(Times, left, right) -> {
      // Make sure both operands are TNums
      use _ <- try(check(left, TNum, scope))
      use _ <- try(check(right, TNum, scope))
      // The resulting type of the expression is a TNum
      Ok(TNum)
    }

    BinOp(Less, left, right) | BinOp(Greater, left, right) -> {
      use _ <- try(check(left, TBool, scope))
      use _ <- try(check(right, TBool, scope))
      Ok(TBool)
    }

    BinOp(Concat, left, right) -> {
      use _ <- try(check(left, TStr, scope))
      use _ <- try(check(right, TStr, scope))
      Ok(TStr)
    }

    If(cond, then, otherwise) -> {
      // The condition must be a TBool
      use _ <- try(check(cond, TBool, scope))
      use then_type <- try(type_of(then, scope))
      // Both branches must evaluate to the same type
      use _ <- try(check(otherwise, then_type, scope))
      // And the entire expression evaluates to that type
      Ok(then_type)
    }

    Fun(name, args, return_type, body) -> {
      let function_type = TFun(dict.values(args), return_type)
      // Update the scope to include the function itself (so it can call itself)
      // and its arguments
      let scope =
        scope
        |> dict.insert(name, function_type)
        |> dict.fold(args, _, dict.insert)

      // Make sure the body is the right type
      use _ <- try(check(body, return_type, scope))

      Ok(function_type)
    }

    Apply(expr, args) -> {
      use ty <- try(type_of(expr, scope))

      // First we make sure the expression that we are calling is actually a function
      case ty {
        TFun(in, out) -> {
          // Since the expression being called is a function, validate the arguments
          use _ <- try(check_args(args, in, scope))
          // And return the return type of the function
          Ok(out)
        }
        _ ->
          Error(TypeError(
            "expression was used as a function but its type is "
            <> string.inspect(ty),
          ))
      }
    }
  }
}

/// Verify that an expression (`expr`) has a specific type (`expected`).
fn check(expr: Expr, expected: Type, scope: Scope) -> Result(Nil, TypeError) {
  use ty <- try(type_of(expr, scope))

  case ty == expected {
    True -> Ok(Nil)
    False ->
      Error(TypeError(
        "Expression has type "
        <> string.inspect(ty)
        <> " but is used as if it had type "
        <> string.inspect(expected),
      ))
  }
}

/// Check that the given arguments match the given types, and that the amounts are
/// not mismatched.
fn check_args(
  args: List(Expr),
  types: List(Type),
  scope: Scope,
) -> Result(Nil, TypeError) {
  case args, types {
    [arg, ..rest_args], [ty, ..rest_types] -> {
      use _ <- try(check(arg, ty, scope))
      check_args(rest_args, rest_types, scope)
    }
    [_, ..], [] -> Error(TypeError("Too many args"))
    [], [_, ..] -> Error(TypeError("Too few args"))
    [], [] -> Ok(Nil)
  }
}

pub fn main() {
  Fun(
    "test",
    dict.from_list([#("blah", TBool)]),
    TStr,
    If(
      Var("blah"),
      then: Str("Yeah!"),
      otherwise: Apply(Var("test"), [Bool(True)]),
    ),
  )
  |> Apply([Bool(False)])
  |> type_of(dict.new())
  |> io.debug
}
