import gleam
import gleam/io
import gleam/bool
import gleam/list
import gleam/dict.{type Dict}
import gleam/string
import gleam/result.{try}

pub type Expr {
  EInt(value: Int)
  EString(value: String)
  EBool(value: gleam.Bool)
  EVar(name: String)
  EList(elements: List(Expr))
  EBinOp(op: Op, left: Expr, right: Expr)
  EIf(cond: Expr, then: Expr, otherwise: Expr)
  EFun(
    name: String,
    args: Dict(String, Type),
    generics: List(String),
    return_type: Type,
    body: Expr,
  )
  EApply(expr: Expr, args: List(Expr), generics: List(Type))
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
  TInt
  TString
  TBool
  TList(of: Type)
  TUnknownList
  TFun(in: List(Type), generics: List(String), out: Type)
  TVar(name: String)
  TUnknownVar(name: String)
}

// This type would probably have more info in a real language, such as the position
// of the error, and possibly other constructors.
pub type TypeError {
  TypeError(message: String)
}

type Scope =
  Dict(String, Type)

type TypeVars =
  Dict(String, Type)

/// Find the type of an expression in a given scope.
fn type_of(expr: Expr, scope: Scope, tvars: TypeVars) -> Result(Type, TypeError) {
  case expr {
    // To get the type of a variable, all we have to do is look up its name in the
    // scope dictionary. If it doesn't exist, then we return an error
    EVar(name) ->
      case dict.get(scope, name) {
        Ok(ty) -> Ok(ty)
        Error(_) -> Error(TypeError("Var not defined: " <> name))
      }

    // Literals are super easy to typecheck. They map 1:1 with their types
    EInt(_) -> Ok(TInt)
    EString(_) -> Ok(TString)
    EBool(_) -> Ok(TBool)

    EList(elements) -> {
      case elements {
        [] -> Ok(TUnknownList)
        [first, ..] -> {
          use t <- try(type_of(first, scope, tvars))
          Ok(TList(t))
        }
      }
    }

    EBinOp(Plus, left, right)
    | EBinOp(Minus, left, right)
    | EBinOp(Times, left, right) -> {
      // Make sure both operands are TNums
      use _ <- try(check_expr(left, TInt, scope, tvars))
      use _ <- try(check_expr(right, TInt, scope, tvars))
      // The resulting type of the expression is a TInt
      Ok(TInt)
    }

    EBinOp(Less, left, right) | EBinOp(Greater, left, right) -> {
      use _ <- try(check_expr(left, TBool, scope, tvars))
      use _ <- try(check_expr(right, TBool, scope, tvars))
      Ok(TBool)
    }

    EBinOp(Concat, left, right) -> {
      use _ <- try(check_expr(left, TString, scope, tvars))
      use _ <- try(check_expr(right, TString, scope, tvars))
      Ok(TString)
    }

    EIf(cond, then, otherwise) -> {
      // The condition must be a TBool
      use _ <- try(check_expr(cond, TBool, scope, tvars))
      use then_type <- try(type_of(then, scope, tvars))
      // Both branches must evaluate to the same type
      use _ <- try(check_expr(otherwise, then_type, scope, tvars))
      // And the entire expression evaluates to that type
      Ok(then_type)
    }

    EFun(name, args, generics, return_type, body) -> {
      let function_type = TFun(dict.values(args), generics, return_type)
      // Update the scope to include the function itself (so it can call itself)
      // and its arguments
      let scope =
        scope
        |> dict.insert(name, function_type)
        |> dict.fold(args, _, dict.insert)

      let tvars =
        list.map(generics, fn(g) { #(g, TUnknownVar(g)) })
        |> dict.from_list
        |> dict.merge(tvars, _)

      // Make sure the body is the right type
      use _ <- try(check_expr(body, return_type, scope, tvars))

      Ok(function_type)
    }

    EApply(expr, args, apply_generics) -> {
      use ty <- try(type_of(expr, scope, tvars))

      // First we make sure the expression that we are calling is actually a function
      case ty {
        TFun(in, fun_generics, out) -> {
          // Make sure we give the right amount of arguments to the function...
          use <- bool.guard(
            when: list.length(in) != list.length(args),
            return: Error(TypeError("Wrong arg amounts")),
          )
          // ...and the right amount of generics.
          use <- bool.guard(
            when: list.length(fun_generics) != list.length(apply_generics),
            return: Error(TypeError("Wrong generics amount")),
          )

          // We substitute each of the apply_generics so that they don't contain
          // any type variables.
          use apply_generics <- try(
            list.try_map(apply_generics, substitute(_, tvars)),
          )

          // Add the generics to the param dictionary
          let tvars =
            list.zip(fun_generics, apply_generics)
            |> dict.from_list
            |> dict.merge(tvars, _)

          // Check each argument and make sure it is the right type
          use _ <- try(
            list.zip(args, in)
            |> list.try_each(fn(arg) {
              let #(expr, t) = arg
              check_expr(expr, t, scope, tvars)
            }),
          )

          // And return the return type of the function
          substitute(out, tvars)
        }
        _ ->
          Error(TypeError(
            "Expression was used as a function but its type is "
            <> string.inspect(ty),
          ))
      }
    }
  }
}

/// Verify that an expression (`expr`) has a specific type (`expected`).
fn check_expr(
  expr: Expr,
  expected: Type,
  scope: Scope,
  tvars: TypeVars,
) -> Result(Nil, TypeError) {
  use t <- try(type_of(expr, scope, tvars))
  check(t, expected, tvars)
}

// Verify that `got` is equal to `expected`.
fn check(got: Type, expected: Type, tvars: TypeVars) -> Result(Nil, TypeError) {
  case got, expected {
    TInt, TInt | TString, TString | TBool, TBool -> Ok(Nil)

    TFun(in1, generics1, out1), TFun(in2, generics2, out2) -> {
      use <- bool.guard(
        when: list.length(generics1) != list.length(generics2),
        return: Error(TypeError("Mismatched generics lengths")),
      )

      use _ <- try(
        list.zip(in1, in2)
        |> list.try_each(fn(ins) {
          let #(in1, in2) = ins
          check(in1, in2, tvars)
        }),
      )

      check(out1, out2, tvars)
    }

    TUnknownList, TList(_) -> Ok(Nil)

    TList(a), TList(b) -> check(a, b, tvars)

    TUnknownVar(a), TUnknownVar(b) if a == b -> Ok(Nil)

    TVar(name), _ ->
      case dict.get(tvars, name) {
        Ok(t) -> check(t, expected, tvars)
        Error(_) -> Error(TypeError("Type var not defined: " <> name))
      }

    _, TVar(name) ->
      case dict.get(tvars, name) {
        Ok(t) -> check(got, t, tvars)
        Error(_) -> Error(TypeError("Type var not defined: " <> name))
      }

    _, _ ->
      Error(TypeError(
        "Expression has type "
        <> string.inspect(got)
        <> " but is used as if it had type "
        <> string.inspect(expected),
      ))
  }
}

/// Replace all type variables in the given type with their type values.
fn substitute(t: Type, tvars: TypeVars) -> Result(Type, TypeError) {
  case t {
    TInt | TString | TBool | TUnknownList -> Ok(t)
    TList(t) -> {
      use t <- try(substitute(t, tvars))
      Ok(TList(t))
    }
    TFun(in, generics, out) -> {
      use in <- try(list.try_map(in, substitute(_, tvars)))
      use out <- try(substitute(out, tvars))
      Ok(TFun(in, generics, out))
    }
    TVar(name) ->
      case dict.get(tvars, name) {
        Ok(t) -> Ok(t)
        Error(_) -> Error(TypeError("Type var not defined: " <> name))
      }
    TUnknownVar(n) -> Ok(TUnknownVar(n))
  }
}

pub fn main() {
  // EFun(
  //   name: "test",
  //   args: dict.from_list([#("blah", TVar("a"))]),
  //   generics: [],
  //   return_type: TString,
  //   body: EIf(
  //     EVar("blah"),
  //     then: EString("Yeah!"),
  //     otherwise: EApply(EVar("test"), [EBool(True)], []),
  //   ),
  // )
  // |> EApply([EBool(False)], [])
  EFun(
    name: "test",
    args: dict.from_list([#("foo", TList(TVar("a")))]),
    generics: ["a"],
    return_type: TList(TVar("a")),
    body: EVar("foo"),
  )
  |> EApply([EList([])], [TString])
  |> type_of(dict.new(), dict.new())
  |> io.debug
  // test<a>([1, 2, 3])
}
