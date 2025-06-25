import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string

import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, die, get_frame_offset,
  get_return_type, insert_symbol, perform, pop_frame, pure, push_frame,
  resolve_symbol, resolve_type, return_found, set_frame_offset, set_return_type,
  sub_frame_offset, traverse, traverse_until_return,
}
import starc/codegen/env.{Function, TypeError, Variable}
import starc/parser/ast

fn typify_constants(
  expr: ast.TypedExpression,
  ty: ast.Type,
) -> Generator(ast.TypedExpression, r) {
  case expr {
    ast.TypedIntExpr(value:, ..) -> pure(ast.TypedIntExpr(value:, ty:))

    ast.TypedAddExpr(e1, e2, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedAddExpr(e1:, e2:, ty:))
    }
    ast.TypedSubExpr(e1, e2, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedSubExpr(e1:, e2:, ty:))
    }
    ast.TypedMulExpr(e1, e2, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedMulExpr(e1:, e2:, ty:))
    }
    ast.TypedDivExpr(e1, e2, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedDivExpr(e1:, e2:, ty:))
    }

    _ -> pure(expr)
  }
}

fn unify(ty1: ast.Type, ty2: ast.Type) -> Generator(ast.Type, r) {
  case ty1, ty2 {
    ast.Void, _ | _, ast.Void -> die(TypeError("Cannot unify void type"))

    ast.Bool, ast.Bool -> pure(ast.Bool)

    ast.Int8, ast.Int8 -> pure(ast.Int8)
    ast.Int16, ast.Int16 -> pure(ast.Int16)
    ast.Int32, ast.Int32 -> pure(ast.Int32)
    ast.Int64, ast.Int64 -> pure(ast.Int64)

    ast.IntConst, ty -> pure(ty)
    ty, ast.IntConst -> pure(ty)

    ast.Pointer(ty1), ast.Pointer(ty2) -> {
      use ty <- perform(unify(ty1, ty2))
      pure(ast.Pointer(ty))
    }

    ty1, ty2 ->
      die(TypeError(
        "Cannot unify "
        <> string.inspect(ty1)
        <> " with "
        <> string.inspect(ty2),
      ))
  }
}

fn analyze_expression(
  expression: ast.UntypedExpression,
) -> Generator(ast.TypedExpression, r) {
  case expression {
    ast.UntypedIntExpr(x) -> pure(ast.TypedIntExpr(value: x, ty: ast.IntConst))

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

    // TODO: call any function pointer, pass the address instead of the label
    ast.UntypedCallExpression(f:, args:) -> {
      case f {
        ast.UntypedVarExpr(id) -> {
          use sym <- perform(resolve_symbol(id))
          case sym {
            Function(label:, arg_types:, return_type:) -> {
              use args <- perform(traverse(args, analyze_expression))

              use args <- perform(case list.strict_zip(args, arg_types) {
                Ok(pairs) ->
                  traverse(pairs, fn(pair) {
                    let #(arg, arg_ty) = pair
                    use ty <- perform(unify(ast.type_of(arg), arg_ty))
                    typify_constants(arg, ty)
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

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedAddExpr(e1:, e2:, ty:))
    }

    ast.UntypedSubExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedSubExpr(e1:, e2:, ty:))
    }

    ast.UntypedMulExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedMulExpr(e1:, e2:, ty:))
    }

    ast.UntypedDivExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedDivExpr(e1:, e2:, ty:))
    }

    ast.UntypedNegExpr(expr) -> {
      use expr <- perform(analyze_expression(expr))
      case expr {
        ast.TypedIntExpr(value:, ..) ->
          pure(ast.TypedIntExpr(value: -value, ty: ast.IntConst))

        _ -> {
          let ty = ast.type_of(expr)
          case ty {
            ast.Int8 | ast.Int16 | ast.Int32 | ast.Int64 ->
              pure(ast.TypedNegExpr(e: expr, ty:))

            _ -> die(TypeError("Cannot negate " <> string.inspect(ty)))
          }
        }
      }
    }

    ast.UntypedEQExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedEQExpr(e1:, e2:))
    }

    ast.UntypedNEQExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedNEQExpr(e1:, e2:))
    }

    ast.UntypedLTExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedLTExpr(e1:, e2:))
    }

    ast.UntypedLEExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedLEExpr(e1:, e2:))
    }

    ast.UntypedGTExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedGTExpr(e1:, e2:))
    }

    ast.UntypedGEExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use ty <- perform(unify(ast.type_of(e1), ast.type_of(e2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedGEExpr(e1:, e2:))
    }

    ast.UntypedAndExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use _ <- perform(unify(ast.type_of(e1), ast.Bool))
      use _ <- perform(unify(ast.type_of(e2), ast.Bool))

      pure(ast.TypedAndExpr(e1:, e2:))
    }

    ast.UntypedOrExpr(e1, e2) -> {
      use e1 <- perform(analyze_expression(e1))
      use e2 <- perform(analyze_expression(e2))

      use _ <- perform(unify(ast.type_of(e1), ast.Bool))
      use _ <- perform(unify(ast.type_of(e2), ast.Bool))

      pure(ast.TypedOrExpr(e1:, e2:))
    }

    ast.UntypedNotExpr(e) -> {
      use e <- perform(analyze_expression(e))
      case e {
        ast.TypedBoolExpr(x) -> pure(ast.TypedBoolExpr(!x))

        _ -> {
          use _ <- perform(unify(ast.type_of(e), ast.Bool))
          pure(ast.TypedNotExpr(e))
        }
      }
    }
  }
}

fn analyze_statement(
  statement: ast.UntypedStatement,
) -> Generator(ast.TypedStatement, r) {
  case statement {
    ast.UntypedAssignStatement(cell:, expr:) -> {
      use cell <- perform(analyze_expression(cell))
      use expr <- perform(analyze_expression(expr))

      use ty <- perform(unify(ast.type_of(cell), ast.type_of(expr)))
      use expr <- perform(typify_constants(expr, ty))
      pure(ast.TypedAssignStatement(cell:, expr:))
    }

    ast.UntypedCallStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))
      pure(ast.TypedCallStatement(expr))
    }

    ast.UntypedDefineStatement(name:, typeid:, expr:) -> {
      use expr <- perform(analyze_expression(expr))
      let expr_ty = ast.type_of(expr)

      use expr <- perform(case typeid {
        None -> typify_constants(expr, ast.Int64)
        Some(typeid) -> {
          use define_ty <- perform(resolve_type(typeid))
          use ty <- perform(unify(define_ty, expr_ty))
          typify_constants(expr, ty)
        }
      })
      let ty = ast.type_of(expr)

      use _ <- perform(sub_frame_offset(ast.size_of(ty)))
      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(insert_symbol(name, Variable(frame_offset:, ty:)))

      pure(ast.TypedDefineStatement(
        name: ast.TypedVarExpr(id: name, ty:, frame_offset:),
        expr:,
      ))
    }

    ast.UntypedIfStatement(condition:, block:, elseifs:, elseblock:) -> {
      todo
      // use condition <- perform(analyze_expression(condition))
      // use _ <- perform(unify(ast.type_of(condition), ast.Bool))

      // use block <- perform(traverse(block, analyze_statement))

      // use elseifs <- perform(
      //   traverse(elseifs, fn(x) {
      //     let #(condition, block) = x

      //     use condition <- perform(analyze_expression(condition))
      //     use _ <- perform(unify(ast.type_of(condition), ast.Bool))

      //     use block <- perform(traverse(block, analyze_statement))

      //     pure(#(condition, block))
      //   }),
      // )

      // use elseblock <- perform(case elseblock {
      //   None -> pure(None)

      //   Some(block) ->
      //     traverse(block, analyze_statement)
      //     |> core.map(Some)
      // })

      // pure(ast.TypedIfStatement(condition:, block:, elseifs:, elseblock:))
    }

    ast.UntypedReturnStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))

      use return_type <- perform(get_return_type())
      use ty <- perform(unify(return_type, ast.type_of(expr)))

      use expr <- perform(typify_constants(expr, ty))
      return_found(ast.TypedReturnStatement(expr))
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
      use _ <- perform(set_return_type(return_type))

      use #(body, return_found) <- perform(traverse_until_return(
        body,
        analyze_statement,
      ))
      use _ <- perform(case return_type, return_found {
        ast.Void, _ | _, True -> pure(Nil)
        _, False -> die(TypeError("Function must return"))
      })

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
