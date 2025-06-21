import gleam/list

import starc/analyzer
import starc/codegen/core.{
  type Generator, emit, generate, perform, pure, traverse,
}
import starc/codegen/env.{type CodegenError}
import starc/codegen/ir
import starc/parser/ast

fn generate_expression(expr: ast.TypedExpression) -> Generator(ir.Value, r) {
  case expr {
    ast.TypedIntExpr(value:, ..) -> pure(ir.Immediate(value))

    ast.TypedBoolExpr(True) -> pure(ir.Immediate(1))
    ast.TypedBoolExpr(False) -> pure(ir.Immediate(0))

    ast.TypedVarExpr(ty:, frame_offset:, ..) ->
      pure(ir.Deref(
        value: ir.RBP,
        offset: ir.Immediate(frame_offset),
        multiplier: 1,
        size: ast.size_of(ty),
      ))

    ast.TypedAddrOfExpr(e:, ..) -> {
      case e {
        ast.TypedVarExpr(ty:, frame_offset:, ..) -> {
          let size = ast.size_of(ty)

          use _ <- perform(
            emit([
              ir.Lea(
                to: ir.Register(reg: ir.RegA, size:),
                from: ir.Deref(
                  value: ir.RBP,
                  offset: ir.Immediate(frame_offset),
                  multiplier: 1,
                  size:,
                ),
              ),
            ]),
          )
          pure(ir.Register(reg: ir.RegA, size:))
        }
        _ -> panic
      }
    }

    ast.TypedDerefExpr(e:, ty:) -> {
      use e <- perform(generate_expression(e))
      case e {
        ir.Deref(size:, ..) -> {
          use _ <- perform(
            emit([ir.Move(to: ir.Register(reg: ir.RegA, size:), from: e)]),
          )
          pure(ir.Deref(
            value: ir.Register(reg: ir.RegA, size:),
            offset: ir.Immediate(0),
            multiplier: 1,
            size: ast.size_of(ty),
          ))
        }

        _ ->
          pure(ir.Deref(
            value: e,
            offset: ir.Immediate(0),
            multiplier: 1,
            size: ast.size_of(ty),
          ))
      }
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
            from: ir.Deref(
              value: ir.RBP,
              offset: ir.Immediate(return_frame_offset),
              multiplier: 1,
              size: ast.size_of(return_type),
            ),
          ),
        ]),
      )

      use _ <- perform(
        traverse(list.reverse(args), fn(arg) {
          use arg <- perform(generate_expression(arg))
          emit([ir.Push(arg)])
        }),
      )

      use _ <- perform(emit([ir.Call(ir.LabelAddress(label))]))

      use _ <- perform(
        emit([
          ir.Move(to: ir.RSP, from: aux_register),
          ir.Pop(ir.RSI),
          ir.Pop(aux_register),
        ]),
      )

      pure(ir.Deref(
        value: ir.RBP,
        offset: ir.Immediate(return_frame_offset),
        multiplier: 1,
        size: ast.size_of(return_type),
      ))
    }

    ast.TypedAddExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let out_register = ir.Register(reg: ir.RegA, size:)

      use _ <- perform(emit([ir.Push(aux_register)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Add(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register),
        ]),
      )

      pure(out_register)
    }
    ast.TypedSubExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let out_register = ir.Register(reg: ir.RegA, size:)

      use _ <- perform(emit([ir.Push(aux_register)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Sub(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register),
        ]),
      )

      pure(out_register)
    }
    ast.TypedMulExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let out_register = ir.Register(reg: ir.RegA, size:)

      use _ <- perform(emit([ir.Push(aux_register)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Mul(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register),
        ]),
      )

      pure(out_register)
    }
    ast.TypedDivExpr(e1:, e2:, ty:) -> {
      let size = ast.size_of(ty)
      let aux_register = ir.Register(reg: ir.RegB, size:)
      let out_register = ir.Register(reg: ir.RegA, size:)

      use _ <- perform(emit([ir.Push(aux_register)]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(to: aux_register, from: e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Div(to: aux_register, from: e2),
          ir.Move(to: out_register, from: aux_register),
          ir.Pop(aux_register),
        ]),
      )

      pure(out_register)
    }

    ast.TypedEQExpr(e1:, e2:) -> {
      // let ty = ast.type_of(e1)
      // case ty {
      //   ast.Int64 | ast.Int32 | ast.Int16 | ast.Int8 | ast.Bool -> {
      //     use _ <- perform(emit([ir.Push(ir.Register(ir.RBX))]))

      //     use e1 <- perform(generate_expression(e1))
      //     use _ <- perform(emit([ir.Move(to: ir.Register(ir.RBX), from: e1)]))

      //     use e2 <- perform(generate_expression(e2))
      //     use _ <- perform(
      //       emit([
      //         ir.Cmp(ir.Register(ir.RBX), e2),
      //         ir.ExtractZF(ir.Register(ir.RAX)),
      //         ir.Pop(ir.Register(ir.RBX)),
      //       ]),
      //     )

      //     pure(ir.Register(ir.RAX))
      //   }

      //   ast.Pointer(_) -> todo

      //   ast.Void -> panic
      // }
      todo
    }
    ast.TypedNEQExpr(e1:, e2:) -> {
      todo
      // let ty = ast.type_of(e1)
      // case ty {
      //   ast.Int64 | ast.Int32 | ast.Int16 | ast.Int8 | ast.Bool -> {
      //     use _ <- perform(emit([ir.Push(ir.Register(ir.RBX))]))

      //     use e1 <- perform(generate_expression(e1))
      //     use _ <- perform(emit([ir.Move(to: ir.Register(ir.RBX), from: e1)]))

      //     use e2 <- perform(generate_expression(e2))
      //     use _ <- perform(
      //       emit([
      //         ir.Cmp(ir.Register(ir.RBX), e2),
      //         ir.ExtractZF(ir.Register(ir.RAX)),
      //         ir.Move(to: ir.Register(ir.RBX), from: ir.Immediate(1)),
      //         ir.AndN(
      //           to: ir.Register(ir.RAX),
      //           from: ir.Register(ir.RAX),
      //           mask: ir.Register(ir.RBX),
      //         ),
      //         ir.Pop(ir.Register(ir.RBX)),
      //       ]),
      //     )

      //     pure(ir.Register(ir.RAX))
      //   }

      //   ast.Pointer(_) -> todo

      //   ast.Void -> panic
      // }
    }
    ast.TypedGEExpr(..) -> todo
    ast.TypedGTExpr(..) -> todo
    ast.TypedLEExpr(..) -> todo
    ast.TypedLTExpr(..) -> todo
    ast.TypedNotExpr(..) -> todo
  }
}

fn generate_statement(statement: ast.TypedStatement) -> Generator(Nil, r) {
  case statement {
    ast.TypedAssignStatement(cell:, expr:) -> {
      case cell {
        ast.TypedVarExpr(ty:, frame_offset:, ..) -> {
          use expr <- perform(generate_expression(expr))
          emit([
            ir.Move(
              to: ir.Deref(
                value: ir.RBP,
                offset: ir.Immediate(frame_offset),
                multiplier: 1,
                size: ast.size_of(ty),
              ),
              from: expr,
            ),
          ])
        }

        ast.TypedDerefExpr(ty:, ..) -> {
          let aux_regiser = ir.Register(reg: ir.RegB, size: 8)

          use _ <- perform(emit([ir.Push(aux_regiser)]))

          use cell <- perform(generate_expression(cell))
          use _ <- perform(emit([ir.Lea(to: aux_regiser, from: cell)]))

          use expr <- perform(generate_expression(expr))

          emit([
            ir.Move(
              to: ir.Deref(
                value: aux_regiser,
                offset: ir.Immediate(0),
                multiplier: 1,
                size: ast.size_of(ty),
              ),
              from: expr,
            ),
            ir.Pop(aux_regiser),
          ])
        }

        _ -> panic
      }
    }

    ast.TypedCallStatement(expr) -> {
      use _ <- perform(generate_expression(expr))
      pure(Nil)
    }

    ast.TypedDefineStatement(name:, expr:) -> {
      let assert ast.TypedVarExpr(ty:, frame_offset:, ..) = name

      use expr <- perform(generate_expression(expr))
      emit([
        ir.Move(
          to: ir.Deref(
            value: ir.RBP,
            offset: ir.Immediate(frame_offset),
            multiplier: 1,
            size: ast.size_of(ty),
          ),
          from: expr,
        ),
      ])
    }

    ast.TypedIfStatement(condition:, block:, elseifs:, elseblock:) -> todo

    ast.TypedReturnStatement(expr) -> {
      let size = ast.type_of(expr) |> ast.size_of()

      use expr <- perform(generate_expression(expr))
      emit([
        ir.Move(
          to: ir.Deref(
            value: ir.RSI,
            offset: ir.Immediate(0),
            multiplier: 1,
            size:,
          ),
          from: expr,
        ),
      ])
    }
  }
}

fn generate_function(declaration: ast.TypedDeclaration) -> Generator(Nil, r) {
  let ast.TypedFunctionDeclaration(label:, body:, reserve_bytes:) = declaration

  use _ <- perform(emit([ir.Label(label), ir.Prologue(reserve_bytes:)]))

  use _ <- perform(traverse(body, generate_statement))

  use _ <- perform(emit([ir.Epilogue]))

  pure(Nil)
}

pub fn generate_program(
  program: ast.UntypedProgram,
) -> Result(ir.Program, CodegenError) {
  let g = {
    use tree <- perform(analyzer.analyze_program(program))
    traverse(tree, generate_function)
  }

  generate(g)
}
