import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair

import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, die, get_frame_offset,
  get_return_type, insert_symbol, perform, pop_frame, pure, push_frame,
  resolve_symbol, resolve_type, return_found, set_frame_offset, set_return_type,
  sub_frame_offset, traverse, traverse_until_return,
}
import starc/codegen/env.{
  type Environment, type SemanticError, Function, Variable,
}
import starc/lexer/token.{type Span}
import starc/parser/ast

fn typify_constants(
  expr: ast.TypedExpression,
  ty: ast.Type,
) -> Generator(ast.TypedExpression, r) {
  case expr {
    ast.TypedIntExpr(value:, ty: ast.IntConst) ->
      pure(ast.TypedIntExpr(value:, ty:))

    ast.TypedAddExpr(e1:, e2:, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedAddExpr(e1:, e2:, ty:))
    }

    ast.TypedSubExpr(e1:, e2:, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedSubExpr(e1:, e2:, ty:))
    }

    ast.TypedMulExpr(e1:, e2:, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedMulExpr(e1:, e2:, ty:))
    }

    ast.TypedDivExpr(e1:, e2:, ty: ast.IntConst) -> {
      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))
      pure(ast.TypedDivExpr(e1:, e2:, ty:))
    }

    _ -> pure(expr)
  }
}

fn unify(
  actual: ast.Type,
  actual_span: Span,
  expected: ast.Type,
  expected_span: Option(Span),
) -> Generator(ast.Type, r) {
  case actual, expected {
    ast.Void, _ | _, ast.Void ->
      die(env.TypeMismatch(actual:, actual_span:, expected:, expected_span:))

    ast.Bool, ast.Bool -> pure(ast.Bool)

    ast.Int8, ast.Int8 -> pure(ast.Int8)
    ast.Int16, ast.Int16 -> pure(ast.Int16)
    ast.Int32, ast.Int32 -> pure(ast.Int32)
    ast.Int64, ast.Int64 -> pure(ast.Int64)

    ast.IntConst, ast.Int8 -> pure(ast.Int8)
    ast.IntConst, ast.Int16 -> pure(ast.Int16)
    ast.IntConst, ast.Int32 -> pure(ast.Int32)
    ast.IntConst, ast.Int64 -> pure(ast.Int64)

    ast.Int8, ast.IntConst -> pure(ast.Int8)
    ast.Int16, ast.IntConst -> pure(ast.Int16)
    ast.Int32, ast.IntConst -> pure(ast.Int32)
    ast.Int64, ast.IntConst -> pure(ast.Int64)

    ast.IntConst, ast.IntConst -> pure(ast.IntConst)

    ast.Pointer(actual), ast.Pointer(expected) -> {
      use ty <- perform(unify(actual, actual_span, expected, expected_span))
      pure(ast.Pointer(ty))
    }

    _, _ ->
      die(env.TypeMismatch(actual:, actual_span:, expected:, expected_span:))
  }
}

fn expect_int(ty: ast.Type, span: Span) -> Generator(Nil, r) {
  case ty {
    ast.Int8 | ast.Int16 | ast.Int32 | ast.Int64 | ast.IntConst -> pure(Nil)
    _ -> die(env.NotInteger(ty:, span:))
  }
}

fn analyze_expression(
  expression: ast.UntypedExpression,
) -> Generator(ast.TypedExpression, r) {
  case expression {
    ast.UntypedIntExpr(value:, ..) ->
      pure(ast.TypedIntExpr(value:, ty: ast.IntConst))

    ast.UntypedBoolExpr(value:, ..) -> pure(ast.TypedBoolExpr(value))

    ast.UntypedStringExpr(..) -> todo

    ast.UntypedVarExpr(id) -> {
      use sym <- perform(resolve_symbol(id))
      case sym {
        Function(..) -> die(env.FunctionAsValue(id))

        Variable(frame_offset:, ty:) ->
          pure(ast.TypedVarExpr(ty:, frame_offset:))
      }
    }

    //TODO: address of other things
    ast.UntypedAddrOfExpr(e:, ..) -> {
      let span = ast.span_of(e)
      use e <- perform(analyze_expression(e))
      case e {
        ast.TypedVarExpr(ty:, ..) ->
          pure(ast.TypedAddrOfExpr(e:, ty: ast.Pointer(ty)))

        _ -> die(env.AddressNotOfVariable(span))
      }
    }

    ast.UntypedDerefExpr(e:, ..) -> {
      let span = ast.span_of(e)
      use e <- perform(analyze_expression(e))
      case ast.type_of(e) {
        ast.Pointer(ty) -> pure(ast.TypedDerefExpr(e:, ty:))

        ty -> die(env.DerefNotPointer(ty:, span:))
      }
    }

    // TODO: call any function pointer, pass the address instead of the label
    ast.UntypedCallExpression(f:, args:, span:) -> {
      case f {
        ast.UntypedVarExpr(id) -> {
          use sym <- perform(resolve_symbol(id))
          case sym {
            Function(label:, arg_types:, return_type:) -> {
              let expected_count = list.length(arg_types)
              let actual_count = list.length(args)

              use _ <- perform(case expected_count == actual_count {
                True -> pure(Nil)

                False ->
                  die(env.CallArgumentCountMismatch(
                    expected: expected_count,
                    actual: actual_count,
                    span:,
                  ))
              })

              use args <- perform(
                traverse(list.zip(args, arg_types), fn(x) {
                  let #(arg, arg_ty) = x

                  let arg_span = ast.span_of(arg)
                  use arg <- perform(analyze_expression(arg))

                  use ty <- perform(unify(
                    ast.type_of(arg),
                    arg_span,
                    arg_ty,
                    None,
                  ))
                  typify_constants(arg, ty)
                }),
              )

              use _ <- perform(sub_frame_offset(ast.size_of(return_type)))
              use frame_offset <- perform(get_frame_offset())

              pure(ast.TypedCallExpression(
                label:,
                args:,
                return_type:,
                return_frame_offset: frame_offset,
              ))
            }

            _ -> die(env.CallNotFunction(id.span))
          }
        }

        _ -> die(env.WrongCallExpression(ast.span_of(f)))
      }
    }

    ast.UntypedAddExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedAddExpr(e1:, e2:, ty:))
    }

    ast.UntypedSubExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedSubExpr(e1:, e2:, ty:))
    }

    ast.UntypedMulExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedMulExpr(e1:, e2:, ty:))
    }

    ast.UntypedDivExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedDivExpr(e1:, e2:, ty:))
    }

    ast.UntypedNegExpr(e:, ..) -> {
      let span = ast.span_of(e)
      use e <- perform(analyze_expression(e))
      let ty = ast.type_of(e)

      use _ <- perform(expect_int(ty, span))

      case e {
        ast.TypedIntExpr(value:, ..) ->
          pure(ast.TypedIntExpr(value: -value, ty: ast.IntConst))

        _ -> pure(ast.TypedNegExpr(e:, ty:))
      }
    }

    ast.UntypedEQExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedEQExpr(e1:, e2:))
    }

    ast.UntypedNEQExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedNEQExpr(e1:, e2:))
    }

    ast.UntypedGTExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedGTExpr(e1:, e2:))
    }

    ast.UntypedGEExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedGEExpr(e1:, e2:))
    }

    ast.UntypedLEExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedLEExpr(e1:, e2:))
    }

    ast.UntypedLTExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))
      let ty1 = ast.type_of(e1)

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))
      let ty2 = ast.type_of(e2)

      use _ <- perform(expect_int(ty1, span1))
      use _ <- perform(expect_int(ty2, span2))

      use ty <- perform(unify(ty1, span1, ty2, Some(span2)))
      let ty = case ty {
        ast.IntConst -> ast.Int64
        _ -> ty
      }

      use e1 <- perform(typify_constants(e1, ty))
      use e2 <- perform(typify_constants(e2, ty))

      pure(ast.TypedLTExpr(e1:, e2:))
    }

    ast.UntypedAndExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))

      use _ <- perform(unify(ast.type_of(e1), span1, ast.Bool, None))
      use _ <- perform(unify(ast.type_of(e2), span2, ast.Bool, None))

      pure(ast.TypedAndExpr(e1:, e2:))
    }

    ast.UntypedOrExpr(e1:, e2:, ..) -> {
      let span1 = ast.span_of(e1)
      use e1 <- perform(analyze_expression(e1))

      let span2 = ast.span_of(e2)
      use e2 <- perform(analyze_expression(e2))

      use _ <- perform(unify(ast.type_of(e1), span1, ast.Bool, None))
      use _ <- perform(unify(ast.type_of(e2), span2, ast.Bool, None))

      pure(ast.TypedOrExpr(e1:, e2:))
    }

    ast.UntypedNotExpr(e, ..) -> {
      let span = ast.span_of(e)
      use e <- perform(analyze_expression(e))
      case e {
        ast.TypedBoolExpr(x) -> pure(ast.TypedBoolExpr(!x))

        _ -> {
          use _ <- perform(unify(ast.type_of(e), span, ast.Bool, None))
          pure(ast.TypedNotExpr(e))
        }
      }
    }
  }
}

// FIXMEEEEEE
fn specialized_analyze_statement(
  statement: ast.UntypedStatement,
) -> Generator(
  ast.TypedStatement,
  Result(#(Environment, List(b), Bool), SemanticError),
) {
  analyze_statement(statement)
}

fn analyze_statement(
  statement: ast.UntypedStatement,
) -> Generator(ast.TypedStatement, r) {
  case statement {
    ast.UntypedAssignStatement(cell:, expr:) -> {
      let cell_span = ast.span_of(cell)
      use cell <- perform(analyze_expression(cell))

      let expr_span = ast.span_of(expr)
      use expr <- perform(analyze_expression(expr))

      use ty <- perform(unify(
        ast.type_of(expr),
        expr_span,
        ast.type_of(cell),
        Some(cell_span),
      ))
      use expr <- perform(typify_constants(expr, ty))
      pure(ast.TypedAssignStatement(cell:, expr:))
    }

    ast.UntypedCallStatement(expr) -> {
      use expr <- perform(analyze_expression(expr))
      pure(ast.TypedCallStatement(expr))
    }

    ast.UntypedDefineStatement(id:, typeid:, expr:) -> {
      let expr_span = ast.span_of(expr)
      use expr <- perform(analyze_expression(expr))

      use expr <- perform(case typeid {
        None -> typify_constants(expr, ast.Int64)

        Some(typeid) -> {
          use define_ty <- perform(resolve_type(typeid))

          use ty <- perform(unify(
            ast.type_of(expr),
            expr_span,
            define_ty,
            Some(typeid.span),
          ))
          typify_constants(expr, ty)
        }
      })
      let ty = ast.type_of(expr)

      use _ <- perform(sub_frame_offset(ast.size_of(ty)))
      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(insert_symbol(id, Variable(frame_offset:, ty:)))

      pure(ast.TypedDefineStatement(
        var: ast.TypedVarExpr(ty:, frame_offset:),
        expr:,
      ))
    }

    ast.UntypedIfStatement(condition:, block:, elseifs:, elseblock:) -> {
      let condition_span = ast.span_of(condition)

      use condition <- perform(analyze_expression(condition))
      use _ <- perform(unify(
        ast.type_of(condition),
        condition_span,
        ast.Bool,
        None,
      ))

      use _ <- perform(push_frame())
      use #(block, if_return_found) <- perform(traverse_until_return(
        block,
        specialized_analyze_statement,
      ))
      use _ <- perform(pop_frame())

      use elseifs <- perform(
        traverse(elseifs, fn(x) {
          let #(condition, block) = x

          let condition_span = ast.span_of(condition)

          use condition <- perform(analyze_expression(condition))
          use _ <- perform(unify(
            ast.type_of(condition),
            condition_span,
            ast.Bool,
            None,
          ))

          use _ <- perform(push_frame())
          use #(block, return_found) <- perform(traverse_until_return(
            block,
            specialized_analyze_statement,
          ))
          use _ <- perform(pop_frame())

          pure(#(condition, block, return_found))
        }),
      )

      let #(elseifs_return_found, elseifs) =
        list.map_fold(elseifs, True, fn(acc, x) {
          let #(condition, block, return_found) = x
          #(acc && return_found, #(condition, block))
        })

      use #(elseblock, elseblock_return_found) <- perform(case elseblock {
        None -> pure(#(None, False))

        Some(block) -> {
          use _ <- perform(push_frame())
          use #(block, return_found) <- perform(traverse_until_return(
            block,
            specialized_analyze_statement,
          ))
          use _ <- perform(pop_frame())

          pure(#(Some(block), return_found))
        }
      })

      case if_return_found && elseifs_return_found && elseblock_return_found {
        True ->
          return_found(ast.TypedIfStatement(
            condition:,
            block:,
            elseifs:,
            elseblock:,
          ))

        False ->
          pure(ast.TypedIfStatement(condition:, block:, elseifs:, elseblock:))
      }
    }

    ast.UntypedReturnStatement(expr) -> {
      let expr_span = ast.span_of(expr)
      use expr <- perform(analyze_expression(expr))

      use return_type <- perform(get_return_type())
      use ty <- perform(unify(ast.type_of(expr), expr_span, return_type, None))

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
    ast.UntypedFunctionDeclaration(id:, parameters:, result:, body:, span:) -> {
      use _ <- perform(assert_unique_symbol(id))

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
        id,
        Function(
          label: id.name,
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
        specialized_analyze_statement,
      ))

      use _ <- perform(case return_type, return_found {
        ast.Void, _ | _, True -> pure(Nil)
        _, False -> die(env.MissingReturn(ty: return_type, span:))
      })

      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(pop_frame())

      pure(ast.TypedFunctionDeclaration(
        label: id.name,
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
