import gleam/list

import starc/analyzer
import starc/codegen/core.{
  type Generator, emit, generate, perform, pure, traverse,
}
import starc/codegen/env.{type CodegenError}
import starc/codegen/ir
import starc/parser/ast

// TODO: register allocation
fn generate_expression(expr: ast.TypedExpression) -> Generator(ir.Value, r) {
  case expr {
    ast.TypedIntExpr(value:, ..) -> pure(ir.Immediate(value))

    ast.TypedBoolExpr(value: True, ..) -> pure(ir.Immediate(1))
    ast.TypedBoolExpr(value: False, ..) -> pure(ir.Immediate(0))

    ast.TypedStringExpr(..) -> todo

    ast.TypedVarExpr(ty:, frame_offset:, ..) ->
      pure(ir.Deref(
        value: ir.Register(ir.RBP),
        offset: ir.Immediate(frame_offset),
        multiplier: 1,
        size: ast.size_of(ty),
      ))

    ast.TypedAddrOfExpr(e:, ..) -> {
      case e {
        ast.TypedVarExpr(ty:, frame_offset:, ..) -> {
          use _ <- perform(
            emit([
              ir.Lea(
                ir.Register(ir.RAX),
                ir.Deref(
                  value: ir.Register(ir.RBP),
                  offset: ir.Immediate(frame_offset),
                  multiplier: 1,
                  size: ast.size_of(ty),
                ),
              ),
            ]),
          )
          pure(ir.Register(ir.RAX))
        }
        _ -> panic
      }
    }

    ast.TypedDerefExpr(e:, ty:) -> {
      use e <- perform(generate_expression(e))
      case e {
        ir.Deref(..) -> {
          use _ <- perform(emit([ir.Move(to: ir.Register(ir.RAX), from: e)]))
          pure(ir.Deref(
            value: ir.Register(ir.RAX),
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

    ast.TypedCallExpression(
      label:,
      args:,
      ty: return_type,
      return_frame_offset:,
    ) -> {
      use _ <- perform(
        emit([
          ir.Push(ir.Register(ir.RBX)),
          ir.Push(ir.Register(ir.RSI)),
          ir.Move(to: ir.Register(ir.RBX), from: ir.Register(ir.RSP)),
          ir.Lea(
            to: ir.Register(ir.RSI),
            from: ir.Deref(
              value: ir.Register(ir.RBP),
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
          ir.Move(ir.Register(ir.RSP), ir.Register(ir.RBX)),
          ir.Pop(ir.Register(ir.RSI)),
          ir.Pop(ir.Register(ir.RBX)),
        ]),
      )

      pure(ir.Deref(
        value: ir.Register(ir.RBP),
        offset: ir.Immediate(return_frame_offset),
        multiplier: 1,
        size: ast.size_of(return_type),
      ))
    }

    ast.TypedAddExpr(e1:, e2:, ..) -> {
      use _ <- perform(emit([ir.Push(ir.Register(ir.RBX))]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(ir.Register(ir.RBX), e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Add(ir.Register(ir.RBX), e2),
          ir.Move(ir.Register(ir.RAX), ir.Register(ir.RBX)),
        ]),
      )

      use _ <- perform(emit([ir.Pop(ir.Register(ir.RBX))]))

      pure(ir.Register(ir.RAX))
    }
    ast.TypedSubExpr(e1:, e2:, ..) -> {
      use _ <- perform(emit([ir.Push(ir.Register(ir.RBX))]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(ir.Register(ir.RBX), e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Sub(ir.Register(ir.RBX), e2),
          ir.Move(ir.Register(ir.RAX), ir.Register(ir.RBX)),
        ]),
      )

      use _ <- perform(emit([ir.Pop(ir.Register(ir.RBX))]))

      pure(ir.Register(ir.RAX))
    }
    ast.TypedMulExpr(e1:, e2:, ..) -> {
      use _ <- perform(emit([ir.Push(ir.Register(ir.RBX))]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(ir.Register(ir.RBX), e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Mul(ir.Register(ir.RBX), e2),
          ir.Move(ir.Register(ir.RAX), ir.Register(ir.RBX)),
        ]),
      )

      use _ <- perform(emit([ir.Pop(ir.Register(ir.RBX))]))

      pure(ir.Register(ir.RAX))
    }
    ast.TypedDivExpr(e1:, e2:, ..) -> {
      use _ <- perform(emit([ir.Push(ir.Register(ir.RBX))]))

      use e1 <- perform(generate_expression(e1))
      use _ <- perform(emit([ir.Move(ir.Register(ir.RBX), e1)]))

      use e2 <- perform(generate_expression(e2))
      use _ <- perform(
        emit([
          ir.Div(ir.Register(ir.RBX), e2),
          ir.Move(ir.Register(ir.RAX), ir.Register(ir.RBX)),
        ]),
      )

      use _ <- perform(emit([ir.Pop(ir.Register(ir.RBX))]))

      pure(ir.Register(ir.RAX))
    }

    ast.TypedEQExpr(..) -> todo
    ast.TypedNEQExpr(..) -> todo
    ast.TypedGEExpr(..) -> todo
    ast.TypedGTExpr(..) -> todo
    ast.TypedLEExpr(..) -> todo
    ast.TypedLTExpr(..) -> todo
    ast.TypedNotExpr(..) -> todo
  }
}

fn generate_statement(statement: ast.TypedStatement) -> Generator(Nil, r) {
  case statement {
    ast.TypedAssignStatement(cell:, expr:) -> todo

    ast.TypedCallStatement(expr) -> {
      use _ <- perform(generate_expression(expr))
      pure(Nil)
    }

    ast.TypedDefineStatement(name:, expr:) -> {
      let assert ast.TypedVarExpr(ty:, frame_offset:, ..) = name

      use expr <- perform(generate_expression(expr))
      emit([
        ir.Move(
          ir.Deref(
            value: ir.Register(ir.RBP),
            offset: ir.Immediate(frame_offset),
            multiplier: 1,
            size: ast.size_of(ty),
          ),
          expr,
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
            value: ir.Register(ir.RSI),
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
