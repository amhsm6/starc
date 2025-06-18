import gleam/int
import gleam/list
import gleam/string_tree.{type StringTree}

import starc/analyzer
import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, emit, generate,
  get_frame_offset, insert_symbol, perform, pop_frame, pure, push_frame,
  resolve_symbol, set_frame_offset, sub_frame_offset, traverse,
}
import starc/codegen/env.{type CodegenError, Function, Variable}
import starc/codegen/ir
import starc/parser/ast

// TODO: register allocation
fn generate_expression(expr: ast.Expression) -> Generator(ir.Value, r) {
  let assert ast.TypedExpression(expr:, ty:) = expr
  case expr {
    ast.IntExpr(x) -> pure(ir.Immediate(x))

    ast.BoolExpr(True) -> pure(ir.Immediate(1))
    ast.BoolExpr(False) -> pure(ir.Immediate(0))

    ast.StringExpr(_) -> todo

    ast.VarExpr(id) -> {
      use sym <- perform(resolve_symbol(id))
      let assert Variable(frame_offset:, ..) = sym
      pure(ir.Deref(
        value: ir.Register(ir.RBP),
        offset: ir.Immediate(frame_offset),
        multiplier: 1,
        size: ast.size_of(ty),
      ))
    }

    ast.AddrOfExpr(e) -> {
      let assert ast.TypedExpression(expr: ast.VarExpr(id), ..) = e
      let assert ast.Pointer(ty) = ty

      use sym <- perform(resolve_symbol(id))
      let assert Variable(frame_offset:, ..) = sym

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

    ast.DerefExpr(e) -> {
      use value <- perform(generate_expression(e))
      case value {
        ir.Deref(..) -> {
          use _ <- perform(
            emit([ir.Move(to: ir.Register(ir.RAX), from: value)]),
          )
          pure(ir.Deref(
            value: ir.Register(ir.RAX),
            offset: ir.Immediate(0),
            multiplier: 1,
            size: ast.size_of(ty),
          ))
        }

        _ ->
          pure(ir.Deref(
            value:,
            offset: ir.Immediate(0),
            multiplier: 1,
            size: ast.size_of(ty),
          ))
      }
    }

    ast.CallExpression(f:, args:) -> {
      let assert ast.TypedExpression(expr: ast.VarExpr(id), ty: ast.Function) =
        f

      use sym <- perform(resolve_symbol(id))
      let assert Function(label:, return_type:, ..) = sym

      use _ <- perform(
        emit([
          ir.Push(ir.Register(ir.RBX)),
          ir.Move(ir.Register(ir.RBX), ir.Register(ir.RSP)),
        ]),
      )

      use _ <- perform(case return_type {
        ast.Void -> pure(Nil)
        _ ->
          emit([
            ir.Sub(ir.Register(ir.RSP), ir.Immediate(ast.size_of(return_type))),
          ])
      })

      use _ <- perform(
        traverse(list.reverse(args), fn(arg) {
          use arg <- perform(generate_expression(arg))
          emit([ir.Push(arg)])
        }),
      )

      use _ <- perform(emit([ir.Call(ir.LabelAddress(label))]))

      use _ <- perform(case return_type {
        ast.Void -> pure(Nil)
        _ ->
          emit([
            ir.Move(
              ir.Register(ir.RAX),
              ir.Deref(
                ir.Register(ir.RBX),
                offset: ir.Immediate(-ast.size_of(return_type)),
                multiplier: 1,
                size: ast.size_of(return_type),
              ),
            ),
          ])
      })

      use _ <- perform(
        emit([
          ir.Move(ir.Register(ir.RSP), ir.Register(ir.RBX)),
          ir.Pop(ir.Register(ir.RBX)),
        ]),
      )

      pure(ir.Register(ir.RAX))
    }

    ast.AddExpr(e1, e2) -> {
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
    ast.SubExpr(e1, e2) -> {
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
    ast.MulExpr(e1, e2) -> {
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
    ast.DivExpr(e1, e2) -> {
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

    ast.EQExpr(_, _) -> todo
    ast.NEQExpr(_, _) -> todo
    ast.GEExpr(_, _) -> todo
    ast.GTExpr(_, _) -> todo
    ast.LEExpr(_, _) -> todo
    ast.LTExpr(_, _) -> todo
    ast.NotExpr(_) -> todo
  }
}

fn generate_statement(
  statement: ast.Statement,
  return_frame_offset: Int,
) -> Generator(Nil, r) {
  let assert ast.TypedStatement(statement) = statement
  case statement {
    ast.TypedAssignStatement(cell:, expr:) -> todo

    ast.TypedCallStatement(expr) -> {
      use _ <- perform(generate_expression(expr))
      pure(Nil)
    }

    ast.TypedDefineStatement(name:, expr:) -> {
      let assert ast.TypedExpression(expr: ast.VarExpr(id), ty:) = name

      use _ <- perform(sub_frame_offset(ast.size_of(ty)))
      use frame_offset <- perform(get_frame_offset())
      use _ <- perform(insert_symbol(id, Variable(frame_offset:, ty:)))

      use expr <- perform(generate_expression(expr))
      use _ <- perform(
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
        ]),
      )

      pure(Nil)
    }

    ast.TypedIfStatement(condition:, block:, elseifs:, elseblock:) -> todo

    ast.TypedReturnStatement(expr) -> {
      let assert ast.TypedExpression(ty:, ..) = expr

      use expr <- perform(generate_expression(expr))
      emit([
        ir.Move(
          ir.Deref(
            ir.Register(ir.RBP),
            ir.Immediate(return_frame_offset),
            1,
            ast.size_of(ty),
          ),
          expr,
        ),
      ])
    }
  }
}

fn generate_function(declaration: ast.Declaration) -> Generator(Nil, r) {
  let assert ast.TypedDeclaration(ast.TypedFunctionDeclaration(
    name: label,
    body:,
    parameters: args,
    reserve_bytes:,
    ..,
  )) = declaration

  use _ <- perform(push_frame())
  use _ <- perform(set_frame_offset(16))

  use _ <- perform(emit([ir.Label(label), ir.Prologue(reserve_bytes:)]))

  use _ <- perform(
    traverse(args, fn(x) {
      let #(id, ty) = x
      use _ <- perform(assert_unique_symbol(id))

      use frame_offset <- perform(get_frame_offset())
      use _ <- perform(insert_symbol(id, Variable(frame_offset:, ty:)))

      add_frame_offset(ast.size_of(ty))
    }),
  )

  use return_frame_offset <- perform(get_frame_offset())

  use _ <- perform(set_frame_offset(0))
  use _ <- perform(traverse(body, generate_statement(_, return_frame_offset)))

  use _ <- perform(emit([ir.Epilogue]))

  use _ <- perform(pop_frame())

  pure(Nil)
}

pub fn generate_program(tree: ast.Program) -> Result(ir.Program, CodegenError) {
  let g = {
    use tree <- perform(analyzer.analyze_program(tree))
    traverse(tree, generate_function)
  }

  generate(g)
}

fn serialize_value(value: ir.Value) -> StringTree {
  case value {
    ir.Immediate(x) -> string_tree.from_string(int.to_string(x))
    ir.Register(ir.RAX) -> string_tree.from_string("rax")
    ir.Register(ir.RBX) -> string_tree.from_string("rbx")
    ir.Register(ir.RSP) -> string_tree.from_string("rsp")
    ir.Register(ir.RBP) -> string_tree.from_string("rbp")
    ir.LabelAddress(label) -> string_tree.from_string(label)
    ir.Deref(value:, offset:, multiplier:, size:) -> {
      let ptr = case size {
        1 -> string_tree.from_string("byte ptr")
        2 -> string_tree.from_string("word ptr")
        4 -> string_tree.from_string("dword ptr")
        8 -> string_tree.from_string("qword ptr")
        _ -> panic
      }
      string_tree.concat([
        ptr,
        string_tree.from_string(" ["),
        serialize_value(value),
        string_tree.from_string(" + "),
        serialize_value(offset),
        string_tree.from_string(" * "),
        string_tree.from_string(int.to_string(multiplier)),
        string_tree.from_string("]"),
      ])
    }
    _ -> todo
  }
}

fn serialize_statement(statement: ir.Statement) -> StringTree {
  case statement {
    ir.Add(to:, from:) ->
      string_tree.concat([
        string_tree.from_string("add "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])

    ir.Call(x) ->
      string_tree.concat([string_tree.from_string("call "), serialize_value(x)])

    ir.Div(to:, from:) ->
      string_tree.concat([
        string_tree.from_string("push rax\n"),
        string_tree.from_string("mov rax, "),
        serialize_value(to),
        string_tree.from_string("\n"),
        string_tree.from_string("div "),
        serialize_value(from),
        string_tree.from_string("\n"),
        string_tree.from_string("mov "),
        serialize_value(to),
        string_tree.from_string(", rax\n"),
        string_tree.from_string("pop rax"),
      ])

    ir.Epilogue ->
      string_tree.from_strings(["mov rsp, rbp\n", "pop rbp\n", "ret"])

    ir.Label(x) -> string_tree.from_string(x <> ":")

    ir.Lea(to:, from:) ->
      string_tree.concat([
        string_tree.from_string("lea "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])

    ir.Move(to:, from:) ->
      string_tree.concat([
        string_tree.from_string("mov "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])

    ir.Mul(to:, from:) ->
      string_tree.concat([
        string_tree.from_string("push rax\n"),
        string_tree.from_string("mov rax, "),
        serialize_value(to),
        string_tree.from_string("\n"),
        string_tree.from_string("mul "),
        serialize_value(from),
        string_tree.from_string("\n"),
        string_tree.from_string("mov "),
        serialize_value(to),
        string_tree.from_string(", rax\n"),
        string_tree.from_string("pop rax"),
      ])

    ir.Pop(x) ->
      string_tree.concat([string_tree.from_string("pop "), serialize_value(x)])

    ir.Prologue(reserve_bytes:) ->
      string_tree.from_strings([
        "push rbp\n",
        "mov rbp, rsp\n",
        "sub rsp, ",
        int.to_string(reserve_bytes),
      ])

    ir.Push(x) ->
      string_tree.concat([string_tree.from_string("push "), serialize_value(x)])

    ir.Sub(to:, from:) ->
      string_tree.concat([
        string_tree.from_string("sub "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])
  }
}

pub fn serialize_program(code: ir.Program) -> String {
  list.map(code, serialize_statement)
  |> string_tree.join("\n")
  |> string_tree.to_string()
}
