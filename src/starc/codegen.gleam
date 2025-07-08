import gleam/list
import gleam/option.{None, Some}

import starc/analyzer
import starc/codegen/core.{type Generator, emit, generate, generate_label, perform, pure, traverse}
import starc/codegen/env.{type SemanticError}
import starc/codegen/ir
import starc/parser/ast

fn generate_expression(expr: ast.TypedExpression) -> Generator(ir.Value, r) {
  case expr {
    ast.TypedIntExpr(value:, ty:) -> pure(ir.Immediate(value:, size: ast.size_of(ty)))

    ast.TypedBoolExpr(True) -> pure(ir.Immediate(value: 1, size: 1))
    ast.TypedBoolExpr(False) -> pure(ir.Immediate(value: 0, size: 1))

    ast.TypedVarExpr(ty:, frame_offset:) -> pure(ir.deref(ir.RBP, frame_offset, ast.size_of(ty)))

    ast.TypedSliceExpr(ptr:, len:, frame_offset:, ty:) -> {
      use ptr <- perform(generate_expression(ptr))
      use _ <- perform(emit([ir.Move(to: ir.deref(ir.RBP, frame_offset, 8), from: ptr)]))

      use len <- perform(generate_expression(len))
      use _ <- perform(
        emit([
          ir.Move(to: ir.deref(ir.RBP, frame_offset + 8, ir.size_of(len)), from: len)
        ])
      )

      pure(ir.deref(ir.RBP, frame_offset, ast.size_of(ty)))
    }

    ast.TypedAddrOfExpr(e:, ..) -> {
      case e {
        ast.TypedVarExpr(ty:, frame_offset:) -> {
          use _ <- perform(
            emit([
              ir.Lea(to: ir.Register(reg: ir.RegA, size: 8), from: ir.deref(ir.RBP, frame_offset, ast.size_of(ty)))
            ])
          )
          pure(ir.Register(reg: ir.RegA, size: 8))
        }

        ast.TypedSliceExpr(..) -> todo

        ast.TypedIndexExpr(..) -> todo

        _ -> panic
      }
    }

    ast.TypedDerefExpr(e:, ty:) -> {
      use e <- perform(generate_expression(e))
      case e {
        ir.Deref(..) -> {
          use _ <- perform(emit([ir.Move(to: ir.Register(reg: ir.RegA, size: 8), from: e)]))
          pure(ir.deref(ir.Register(reg: ir.RegA, size: 8), 0, ast.size_of(ty)))
        }

        _ -> pure(ir.deref(e, 0, ast.size_of(ty)))
      }
    }

    ast.TypedIndexExpr(slice:, index:, ty:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 8)
      let aux_register = ir.Register(reg: ir.RegB, size: 8)

      use _ <- perform(emit([ir.Push(aux_register)]))

      use slice <- perform(generate_expression(slice))
      use _ <- perform(emit([ir.Lea(to: out_register, from: slice)]))

      use index <- perform(generate_expression(index))
      use _ <- perform(
        emit([
          ir.Move(to: aux_register, from: index),
          ir.Cmp(aux_register, ir.deref(out_register, 8, 8)),
          ir.JGE(ir.LabelAddress("slice_oob")),
          ir.Move(to: out_register, from: ir.deref(out_register, 0, 8)),
          ir.Lea(
            to: out_register,
            from: ir.Deref(
              value:      out_register,
              offset:     aux_register,
              multiplier: ast.size_of(ty),
              size:       ast.size_of(ty)
            )
          ),
          ir.Pop(aux_register)
        ])
      )

      pure(ir.deref(out_register, 0, ast.size_of(ty)))
    }

    ast.TypedCallExpression(label:, args:, return_type:, return_frame_offset:) -> {
      let aux_register = ir.Register(reg: ir.RegB, size: 8)

      use _ <- perform(
        emit([
          ir.Push(aux_register),
          ir.Push(ir.RSI),
          ir.Move(to: aux_register, from: ir.RSP),
          ir.Lea(
            to: ir.RSI,
            from: ir.deref(ir.RBP, return_frame_offset, ast.size_of(return_type))
          )
        ])
      )

      use _ <- perform(
        traverse(list.reverse(args), fn(arg) {
          use arg <- perform(generate_expression(arg))
          emit([ir.Push(arg)])
        })
      )

      use _ <- perform(emit([ir.Call(ir.LabelAddress(label))]))

      use _ <- perform(
        emit([
          ir.Move(to: ir.RSP, from: aux_register),
          ir.Pop(ir.RSI),
          ir.Pop(aux_register)
        ])
      )

      pure(ir.deref(ir.RBP, return_frame_offset, ast.size_of(return_type)))
    }

    ast.TypedAddExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let out_register = ir.Register(reg: ir.RegA, size:)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Add(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedSubExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let out_register = ir.Register(reg: ir.RegA, size:)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Sub(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedMulExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let out_register = ir.Register(reg: ir.RegA, size:)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Mul(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedDivExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let out_register = ir.Register(reg: ir.RegA, size:)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Div(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedNegExpr(e:, ..) -> {
      use e <- perform(generate_expression(e))
      use _ <- perform(emit([ir.Neg(e)]))
      pure(e)
    }

    ast.TypedEQExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Cmp(aux_register, e2),
          ir.ExtractZF(out_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedNEQExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Cmp(aux_register, e2),
          ir.ExtractZF(out_register),
          ir.Not(out_register),
          ir.And(to: out_register, from: ir.Immediate(value: 1, size: 1)),
          ir.Pop(aux_register_save)
        ]),
      )

      pure(out_register)
    }

    ast.TypedGTExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Cmp(aux_register, e2),
          ir.JGT(ir.LabelAddress("1f")),
          ir.Move(to: out_register, from: ir.Immediate(value: 0, size: 1)),
          ir.Jump(ir.LabelAddress("2f")),
          ir.Label("1"),
          ir.Move(to: out_register, from: ir.Immediate(value: 1, size: 1)),
          ir.Label("2"),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedGEExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Cmp(aux_register, e2),
          ir.JGE(ir.LabelAddress("1f")),
          ir.Move(to: out_register, from: ir.Immediate(value: 0, size: 1)),
          ir.Jump(ir.LabelAddress("2f")),
          ir.Label("1"),
          ir.Move(to: out_register, from: ir.Immediate(value: 1, size: 1)),
          ir.Label("2"),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedLTExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Cmp(aux_register, e2),
          ir.JLT(ir.LabelAddress("1f")),
          ir.Move(to: out_register, from: ir.Immediate(value: 0, size: 1)),
          ir.Jump(ir.LabelAddress("2f")),
          ir.Label("1"),
          ir.Move(to: out_register, from: ir.Immediate(value: 1, size: 1)),
          ir.Label("2"),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedLEExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Cmp(aux_register, e2),
          ir.JLE(ir.LabelAddress("1f")),
          ir.Move(to: out_register, from: ir.Immediate(value: 0, size: 1)),
          ir.Jump(ir.LabelAddress("2f")),
          ir.Label("1"),
          ir.Move(to: out_register, from: ir.Immediate(value: 1, size: 1)),
          ir.Label("2"),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedAndExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.And(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedOrExpr(e1:, e2:) -> {
      let out_register = ir.Register(reg: ir.RegA, size: 1)

      let size = ast.type_of(e1) |> ast.size_of()
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let aux_register_save = ir.Register(..aux_register, size: 8)

      use _ <- perform(emit([ir.Push(aux_register_save)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Or(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register_save)
        ])
      )

      pure(out_register)
    }

    ast.TypedNotExpr(e) -> {
      use e <- perform(generate_expression(e))
      use _ <- perform(emit([ir.Not(e), ir.And(e, ir.Immediate(1, 1))]))
      pure(e)
    }
  }
}

fn generate_statement(statement: ast.TypedStatement) -> Generator(Nil, r) {
  case statement {
    ast.TypedAssignStatement(cell:, expr:) -> {
      case cell {
        ast.TypedVarExpr(ty:, frame_offset:) -> {
          use expr <- perform(generate_expression(expr))
          emit([
            ir.Move(to: ir.deref(ir.RBP, frame_offset, ast.size_of(ty)), from: expr)
          ])
        }

        ast.TypedDerefExpr(ty:, ..) -> {
          let aux_regiser = ir.Register(reg: ir.RegB, size: 8)

          use _ <- perform(emit([ir.Push(aux_regiser)]))

          use cell <- perform(generate_expression(cell))
          use _ <- perform(emit([ir.Lea(to: aux_regiser, from: cell)]))

          use expr <- perform(generate_expression(expr))

          emit([
            ir.Move(to: ir.deref(aux_regiser, 0, ast.size_of(ty)), from: expr),
            ir.Pop(aux_regiser)
          ])
        }

        _ -> panic
      }
    }

    ast.TypedCallStatement(expr) -> {
      use _ <- perform(generate_expression(expr))
      pure(Nil)
    }

    ast.TypedDefineStatement(var:, expr:) -> {
      let assert ast.TypedVarExpr(ty:, frame_offset:) = var

      use expr <- perform(generate_expression(expr))
      emit([
        ir.Move(to: ir.deref(ir.RBP, frame_offset, ast.size_of(ty)), from: expr),
      ])
    }

    ast.TypedIfStatement(condition:, block:, elseifs:, elseblock:) -> {
      use end_label <- perform(generate_label())

      use elseifs_labels <- perform(traverse(elseifs, fn(_) { generate_label() }))
      use else_label <- perform(generate_label())
      let alt_labels = list.append(elseifs_labels, [else_label])

      let assert Ok(false_label) = list.first(alt_labels)
      let alt_labels = list.window_by_2(alt_labels)

      use condition <- perform(generate_expression(condition))
      use _ <- perform(
        emit([
          ir.Cmp(condition, ir.Immediate(value: 1, size: ir.size_of(condition))),
          ir.JNE(ir.LabelAddress(false_label))
        ])
      )

      use _ <- perform(traverse(block, generate_statement))
      use _ <- perform(emit([ir.Jump(ir.LabelAddress(end_label))]))

      use _ <- perform(
        traverse(list.zip(elseifs, alt_labels), fn(x) {
          let #(#(condition, block), #(prev_false_label, next_false_label)) = x

          use _ <- perform(emit([ir.Label(prev_false_label)]))

          use condition <- perform(generate_expression(condition))
          use _ <- perform(
            emit([
              ir.Cmp(
                condition,
                ir.Immediate(value: 1, size: ir.size_of(condition)),
              ),
              ir.JNE(ir.LabelAddress(next_false_label))
            ])
          )

          use _ <- perform(traverse(block, generate_statement))
          emit([ir.Jump(ir.LabelAddress(end_label))])
        }),
      )

      use _ <- perform(emit([ir.Label(else_label)]))
      use _ <- perform(case elseblock {
        None -> pure([])
        Some(block) -> traverse(block, generate_statement)
      })

      emit([ir.Label(end_label)])
    }

    ast.TypedReturnStatement(expr) -> {
      let size = ast.type_of(expr) |> ast.size_of()

      use expr <- perform(generate_expression(expr))
      emit([
        ir.Move(to: ir.deref(ir.RSI, 0, size), from: expr),
        ir.Jump(ir.LabelAddress("9f"))
      ])
    }
  }
}

fn generate_function(declaration: ast.TypedDeclaration) -> Generator(Nil, r) {
  let ast.TypedFunctionDeclaration(label:, body:, reserve_bytes:) = declaration

  use _ <- perform(emit([ir.Label(label), ir.Prologue(reserve_bytes:)]))

  use _ <- perform(traverse(body, generate_statement))

  use _ <- perform(emit([ir.Label("9"), ir.Epilogue]))

  pure(Nil)
}

pub fn generate_program(program: ast.UntypedProgram) -> Result(ir.Program, SemanticError) {
  let g = {
    use tree <- perform(analyzer.analyze_program(program))
    traverse(tree, generate_function)
  }

  generate(g)
}
