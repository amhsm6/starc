import gleam/list
import gleam/option.{None, Some}
import gleam/pair

import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, die, get_frame_offset,
  insert_symbol, perform, pop_frame, pure, push_frame, resolve_symbol,
  resolve_type, set_frame_offset, sub_frame_offset, traverse,
}
import starc/codegen/env.{Function, TypeError, Variable}
import starc/parser/ast

fn same_type(ty1: ast.Type, ty2: ast.Type) -> Bool {
  ty1 == ty2
}

fn analyze_expression(
  expression: ast.UntypedExpression,
) -> Generator(ast.TypedExpression, r) {
  case expression {
    ast.UntypedIntExpr(x) -> pure(ast.TypedIntExpr(value: x, ty: ast.Int64))
    ast.UntypedBoolExpr(x) -> pure(ast.TypedBoolExpr(x))
    ast.UntypedStringExpr(_) -> todo

    ast.UntypedVarExpr(id) -> {
      use sym <- perform(resolve_symbol(id))
      case sym {
        Function(..) -> die(TypeError("Functions cannot be values"))
        Variable(frame_offset:, ty:) ->
          pure(ast.TypedVarExpr(id:, ty:, frame_offset:))
      }
    }

    //TODO: address of other things
    ast.UntypedAddrOfExpr(e) -> {
      use e <- perform(analyze_expression(e))
      case e {
        ast.TypedVarExpr(ty:, ..) ->
          pure(ast.TypedAddrOfExpr(e:, ty: ast.Pointer(ty)))
        _ -> die(TypeError("Can only take address of a variable"))
      }
    }

    ast.UntypedDerefExpr(e) -> {
      use e <- perform(analyze_expression(e))
      case ast.type_of(e) {
        ast.Pointer(ty) -> pure(ast.TypedDerefExpr(e:, ty:))
        _ -> die(TypeError("Can only deref a pointer"))
      }
    }

    ast.UntypedCallExpression(f:, args:) -> {
      case f {
        ast.UntypedVarExpr(id) -> {
          use sym <- perform(resolve_symbol(id))
          case sym {
            Function(label:, arg_types:, return_type:) -> {
              use args <- perform(traverse(args, analyze_expression))

              use _ <- perform(case list.strict_zip(args, arg_types) {
                Ok(pairs) ->
                  traverse(pairs, fn(pair) {
                    let #(arg, arg_ty) = pair
                    case same_type(ast.type_of(arg), arg_ty) {
                      True -> pure(Nil)
                      False -> die(TypeError("Argument type mismatch"))
                    }
                  })
                Error(_) -> die(TypeError("Argument count mismatch"))
              })

              use _ <- perform(sub_frame_offset(ast.size_of(return_type)))
              use frame_offset <- perform(get_frame_offset())

              pure(ast.TypedCallExpression(
                label:,
                args:,
                return_type:,
                return_frame_offset: frame_offset,
              ))
            }
            _ -> die(TypeError("Can only call a function"))
          }
        }
        _ ->
          die(TypeError("Can only call a function by its identifier FIXME??"))
      }
    }

    ast.UntypedAddExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64 -> pure(ast.TypedAddExpr(e1:, e2:, ty: ast.Int64))
        ast.Int32, ast.Int32 -> pure(ast.TypedAddExpr(e1:, e2:, ty: ast.Int32))
        ast.Int16, ast.Int16 -> pure(ast.TypedAddExpr(e1:, e2:, ty: ast.Int16))
        ast.Int8, ast.Int8 -> pure(ast.TypedAddExpr(e1:, e2:, ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedSubExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64 -> pure(ast.TypedSubExpr(e1:, e2:, ty: ast.Int64))
        ast.Int32, ast.Int32 -> pure(ast.TypedSubExpr(e1:, e2:, ty: ast.Int32))
        ast.Int16, ast.Int16 -> pure(ast.TypedSubExpr(e1:, e2:, ty: ast.Int16))
        ast.Int8, ast.Int8 -> pure(ast.TypedSubExpr(e1:, e2:, ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedMulExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64 -> pure(ast.TypedMulExpr(e1:, e2:, ty: ast.Int64))
        ast.Int32, ast.Int32 -> pure(ast.TypedMulExpr(e1:, e2:, ty: ast.Int32))
        ast.Int16, ast.Int16 -> pure(ast.TypedMulExpr(e1:, e2:, ty: ast.Int16))
        ast.Int8, ast.Int8 -> pure(ast.TypedMulExpr(e1:, e2:, ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedDivExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64 -> pure(ast.TypedDivExpr(e1:, e2:, ty: ast.Int64))
        ast.Int32, ast.Int32 -> pure(ast.TypedDivExpr(e1:, e2:, ty: ast.Int32))
        ast.Int16, ast.Int16 -> pure(ast.TypedDivExpr(e1:, e2:, ty: ast.Int16))
        ast.Int8, ast.Int8 -> pure(ast.TypedDivExpr(e1:, e2:, ty: ast.Int8))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedEQExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      case ast.type_of(e1), ast.type_of(e2) {
        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        ty1, ty2 -> {
          case same_type(ty1, ty2) {
            True -> pure(ast.TypedEQExpr(e1:, e2:, ty: ast.Bool))
            False -> die(TypeError("Type mismatch"))
          }
        }
      }
    }

    ast.UntypedNEQExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      case ast.type_of(e1), ast.type_of(e2) {
        ast.Void, _ | _, ast.Void -> die(TypeError("Cannot compare void"))

        ty1, ty2 -> {
          case same_type(ty1, ty2) {
            True -> pure(ast.TypedNEQExpr(e1:, e2:, ty: ast.Bool))
            False -> die(TypeError("Type mismatch"))
          }
        }
      }
    }

    ast.UntypedLTExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64
        | ast.Int32, ast.Int32
        | ast.Int16, ast.Int16
        | ast.Int8, ast.Int8
        -> pure(ast.TypedLTExpr(e1:, e2:, ty: ast.Bool))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedLEExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64
        | ast.Int32, ast.Int32
        | ast.Int16, ast.Int16
        | ast.Int8, ast.Int8
        -> pure(ast.TypedLEExpr(e1:, e2:, ty: ast.Bool))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedGTExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64
        | ast.Int32, ast.Int32
        | ast.Int16, ast.Int16
        | ast.Int8, ast.Int8
        -> pure(ast.TypedGTExpr(e1:, e2:, ty: ast.Bool))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedGEExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))
      case ast.type_of(e1), ast.type_of(e2) {
        ast.Int64, ast.Int64
        | ast.Int32, ast.Int32
        | ast.Int16, ast.Int16
        | ast.Int8, ast.Int8
        -> pure(ast.TypedGEExpr(e1:, e2:, ty: ast.Bool))

        ast.Void, _ | _, ast.Void ->
          die(TypeError("Cannot perform math on void"))

        _, _ -> die(TypeError("Type mismatch"))
      }
    }

    ast.UntypedNotExpr(e) -> {
      use e <- perform(analyze_expression(e))
      case ast.type_of(e) {
        ast.Bool -> pure(ast.TypedNotExpr(e:, ty: ast.Bool))
        ast.Void -> die(TypeError("Cannot negate void"))
        _ -> die(TypeError("Type mismatch"))
      }
    }
  }
}

fn analyze_statement(
  statement: ast.UntypedStatement,
  function_return_type: ast.Type,
) -> Generator(ast.TypedStatement, r) {
  case statement {
    ast.UntypedAssignStatement(cell:, expr:) -> {
      use cell <- perform(analyze_expression(cell))
      use expr <- perform(analyze_expression(expr))

      case same_type(ast.type_of(cell), ast.type_of(expr)) {
        True -> pure(ast.TypedAssignStatement(cell:, expr:))
        False -> die(TypeError("Type mismatch in assignment"))
      }
    }

    ast.UntypedCallStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))
      pure(ast.TypedCallStatement(expr))
    }

    ast.UntypedDefineStatement(name:, typeid:, expr:) -> {
      use expr <- perform(analyze_expression(expr))
      let expr_ty = ast.type_of(expr)

      use ty <- perform(case typeid {
        None -> pure(expr_ty)
        Some(typeid) -> {
          use ty <- perform(resolve_type(typeid))
          case same_type(ty, expr_ty) {
            True -> pure(ty)
            False -> die(TypeError("Type mismatch"))
          }
        }
      })

      use _ <- perform(sub_frame_offset(ast.size_of(ty)))
      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(insert_symbol(name, Variable(frame_offset:, ty:)))

      pure(ast.TypedDefineStatement(
        name: ast.TypedVarExpr(id: name, ty:, frame_offset:),
        expr:,
      ))
    }

    ast.UntypedIfStatement(condition:, block:, elseifs:, elseblock:) -> todo

    ast.UntypedReturnStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))
      case same_type(function_return_type, ast.type_of(expr)) {
        True -> pure(ast.TypedReturnStatement(expr))
        False -> die(TypeError("Return type mismatch"))
      }
    }
  }
}

// FIXME: declaration only sees items defined above
fn analyze_declaration(
  declaration: ast.UntypedDeclaration,
) -> Generator(ast.TypedDeclaration, r) {
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
        Function(
          label: name,
          arg_types: list.map(args, pair.second),
          return_type:,
        ),
      ))

      use _ <- perform(push_frame())
      use _ <- perform(set_frame_offset(16))

      use _ <- perform(
        traverse(args, fn(x) {
          let #(id, ty) = x
          use _ <- perform(assert_unique_symbol(id))

          use frame_offset <- perform(get_frame_offset())
          use _ <- perform(insert_symbol(id, Variable(frame_offset:, ty:)))

          add_frame_offset(ast.size_of(ty))
        }),
      )

      use _ <- perform(set_frame_offset(0))
      use body <- perform(traverse(body, analyze_statement(_, return_type)))
      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(pop_frame())

      pure(ast.TypedFunctionDeclaration(
        label: name,
        body:,
        reserve_bytes: -frame_offset,
      ))
    }
  }
}

pub fn analyze_program(
  tree: ast.UntypedProgram,
) -> Generator(ast.TypedProgram, r) {
  traverse(tree, analyze_declaration)
}
