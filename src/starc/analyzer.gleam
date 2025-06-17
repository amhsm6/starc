import gleam/option.{None, Some}

import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, die, emit, generate,
  get_frame_offset, insert_symbol, perform, pop_frame, pure, push_frame,
  resolve_symbol, resolve_type, set_frame_offset, sub_frame_offset, traverse,
}
import starc/codegen/env.{Function, TypeError, Variable}
import starc/parser/ast

fn same_type(ty1: ast.Type, ty2: ast.Type) -> Bool {
  ty1 == ty2
}

fn analyze_expression(
  expression: ast.Expression,
) -> Generator(ast.Expression, r) {
  let assert ast.UntypedExpression(expr) = expression
  case expr {
    ast.IntExpr(_) -> pure(ast.TypedExpression(expr:, ty: ast.Int64))
    ast.BoolExpr(_) -> pure(ast.TypedExpression(expr:, ty: ast.Bool))
    ast.StringExpr(_) -> todo

    ast.VarExpr(id) -> {
      use sym <- perform(resolve_symbol(id))
      case sym {
        //FIXME?
        Function(..) -> die(TypeError("Functions cannot be values"))
        Variable(ty:, ..) -> pure(ast.TypedExpression(expr:, ty:))
      }
    }

    //FIXME
    ast.AddrOfExpr(e) -> {
      let assert ast.UntypedExpression(e) = e
      case e {
        ast.VarExpr(id) -> {
          use sym <- perform(resolve_symbol(id))
          case sym {
            Variable(ty:, ..) ->
              pure(ast.TypedExpression(
                expr: ast.AddrOfExpr(ast.TypedExpression(expr: e, ty:)),
                ty: ast.Pointer(ty),
              ))
            _ -> die(TypeError("Can only take address of a variable"))
          }
        }
        _ -> die(TypeError("Can only take address of a variable"))
      }
    }

    ast.DerefExpr(e) -> {
      use e <- perform(analyze_expression(e))
      case e {
        ast.TypedExpression(ty: ast.Pointer(ty), ..) ->
          pure(ast.TypedExpression(ast.DerefExpr(e), ty:))
        _ -> die(TypeError("Can only deref a pointer"))
      }
    }

    //FIXME
    ast.CallExpression(f:, args:) -> {
      let assert ast.UntypedExpression(expr) = f
      case expr {
        ast.VarExpr(id) -> {
          use sym <- perform(resolve_symbol(id))
          case sym {
            Function(return_type:, ..) -> {
              use args <- perform(traverse(args, analyze_expression))
              pure(ast.TypedExpression(
                expr: ast.CallExpression(
                  f: ast.TypedExpression(expr:, ty: ast.Function),
                  args:,
                ),
                ty: return_type,
              ))
            }
            _ -> die(TypeError("Can only call a function"))
          }
        }
        _ ->
          die(TypeError("Can only call a function by its identifier FIXME??"))
      }
    }

    ast.AddExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.AddExpr(e1, e2), ty: ast.Int64))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.AddExpr(e1, e2), ty: ast.Int32))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.AddExpr(e1, e2), ty: ast.Int16))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.AddExpr(e1, e2), ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.SubExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.SubExpr(e1, e2), ty: ast.Int64))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.SubExpr(e1, e2), ty: ast.Int32))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.SubExpr(e1, e2), ty: ast.Int16))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.SubExpr(e1, e2), ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.MulExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.MulExpr(e1, e2), ty: ast.Int64))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.MulExpr(e1, e2), ty: ast.Int32))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.MulExpr(e1, e2), ty: ast.Int16))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.MulExpr(e1, e2), ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.DivExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.DivExpr(e1, e2), ty: ast.Int64))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.DivExpr(e1, e2), ty: ast.Int32))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.DivExpr(e1, e2), ty: ast.Int16))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.DivExpr(e1, e2), ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.EQExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        ty1, ty2 -> {
          case same_type(ty1, ty2) {
            True ->
              pure(ast.TypedExpression(expr: ast.EQExpr(e1, e2), ty: ast.Bool))
            False -> die(TypeError("Type mismatch"))
          }
        }
      }
    }

    ast.NEQExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        ty1, ty2 -> {
          case same_type(ty1, ty2) {
            True ->
              pure(ast.TypedExpression(expr: ast.NEQExpr(e1, e2), ty: ast.Bool))
            False -> die(TypeError("Type mismatch"))
          }
        }
      }
    }

    ast.GEExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.GEExpr(e1, e2), ty: ast.Bool))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.GEExpr(e1, e2), ty: ast.Bool))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.GEExpr(e1, e2), ty: ast.Bool))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.GEExpr(e1, e2), ty: ast.Bool))

        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.GTExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.GTExpr(e1, e2), ty: ast.Bool))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.GTExpr(e1, e2), ty: ast.Bool))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.GTExpr(e1, e2), ty: ast.Bool))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.GTExpr(e1, e2), ty: ast.Bool))

        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.LEExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.LEExpr(e1, e2), ty: ast.Bool))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.LEExpr(e1, e2), ty: ast.Bool))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.LEExpr(e1, e2), ty: ast.Bool))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.LEExpr(e1, e2), ty: ast.Bool))

        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.LTExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      let assert ast.TypedExpression(ty: ty1, ..) = e1
      let assert ast.TypedExpression(ty: ty2, ..) = e2

      case ty1, ty2 {
        ast.Int64, ast.Int64 ->
          pure(ast.TypedExpression(expr: ast.LTExpr(e1, e2), ty: ast.Bool))
        ast.Int32, ast.Int32 ->
          pure(ast.TypedExpression(expr: ast.LTExpr(e1, e2), ty: ast.Bool))
        ast.Int16, ast.Int16 ->
          pure(ast.TypedExpression(expr: ast.LTExpr(e1, e2), ty: ast.Bool))
        ast.Int8, ast.Int8 ->
          pure(ast.TypedExpression(expr: ast.LTExpr(e1, e2), ty: ast.Bool))

        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.NotExpr(e) -> {
      use e <- perform(analyze_expression(e))
      let assert ast.TypedExpression(ty: ty, ..) = e
      case ty {
        ast.Bool -> pure(ast.TypedExpression(expr: ast.NotExpr(e), ty:))
        ast.Void -> die(TypeError("Cannot negate void"))
        _ -> die(TypeError("Type mismatch"))
      }
    }
  }
}

fn analyze_statement(
  statement: ast.Statement,
  function_return_type: ast.Type,
) -> Generator(ast.Statement, r) {
  let assert ast.UntypedStatement(statement) = statement
  case statement {
    ast.UntypedAssignStatement(cell:, expr:) -> {
      use cell <- perform(analyze_expression(cell))
      use expr <- perform(analyze_expression(expr))

      let assert ast.TypedExpression(ty: cell_ty, ..) = cell
      let assert ast.TypedExpression(ty: expr_ty, ..) = expr

      case same_type(cell_ty, expr_ty) {
        True -> pure(ast.TypedStatement(ast.TypedAssignStatement(cell:, expr:)))
        False -> die(TypeError("Type mismatch in assignment"))
      }
    }

    ast.UntypedCallStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))
      pure(ast.TypedStatement(ast.TypedCallStatement(expr)))
    }

    ast.UntypedDefineStatement(name:, typeid:, expr:) -> {
      use expr <- perform(analyze_expression(expr))
      let assert ast.TypedExpression(expr:, ty:) = expr

      let assert ast.UntypedExpression(ast.VarExpr(id)) = name

      use ty <- perform(case typeid {
        None -> pure(ty)
        Some(typeid) -> {
          use ty2 <- perform(resolve_type(typeid))
          case same_type(ty, ty2) {
            True -> pure(ty)
            False -> die(TypeError("Type mismatch"))
          }
        }
      })

      use _ <- perform(sub_frame_offset(ast.size_of(ty)))
      use frame_offset <- perform(get_frame_offset())
      use _ <- perform(insert_symbol(id, Variable(frame_offset:, ty:)))

      pure(
        ast.TypedStatement(ast.TypedDefineStatement(
          name: ast.TypedExpression(expr: ast.VarExpr(id), ty:),
          expr: ast.TypedExpression(expr:, ty:),
        )),
      )
    }

    ast.UntypedIfStatement(condition:, block:, elseifs:, elseblock:) -> todo

    ast.UntypedReturnStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))
      let assert ast.TypedExpression(ty:, ..) = expr
      case same_type(function_return_type, ty) {
        True -> pure(ast.TypedStatement(ast.TypedReturnStatement(expr)))
        False -> die(TypeError("Return type mismatch"))
      }
    }
  }
}

// FIXME: declaration only sees items defined above
fn analyze_declaration(
  declaration: ast.Declaration,
) -> Generator(ast.Declaration, r) {
  let assert ast.UntypedDeclaration(declaration) = declaration
  case declaration {
    ast.UntypedFunctionDeclaration(name:, parameters:, result:, body:) -> {
      use _ <- perform(assert_unique_symbol(name))

      use args <- perform(
        traverse(parameters, fn(x) {
          let #(id, typeid) = x
          use ty <- perform(resolve_type(typeid))
          pure(#(id, ty))
        }),
      )

      use return_type <- perform(case result {
        None -> pure(ast.Void)
        Some(typeid) -> resolve_type(typeid)
      })

      use _ <- perform(insert_symbol(
        name,
        Function(label: name, args:, return_type:),
      ))

      use _ <- perform(push_frame())

      use _ <- perform(
        traverse(args, fn(x) {
          let #(id, ty) = x
          use _ <- perform(assert_unique_symbol(id))
          insert_symbol(id, Variable(frame_offset: 0, ty:))
        }),
      )

      use _ <- perform(set_frame_offset(0))
      use body <- perform(traverse(body, analyze_statement(_, return_type)))
      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(pop_frame())

      pure(
        ast.TypedDeclaration(ast.TypedFunctionDeclaration(
          name:,
          parameters: args,
          result: return_type,
          body:,
          reserve_bytes: -frame_offset,
        )),
      )
    }
  }
}

pub fn analyze_program(tree: ast.Program) -> Generator(ast.Program, r) {
  traverse(tree, analyze_declaration)
}
