import gleam/int
import gleam/list
import gleam/string_tree.{type StringTree}

import starc/codegen/ir

fn serialize_value(value: ir.Value) -> StringTree {
  case value {
    ir.Immediate(x) -> string_tree.from_string(int.to_string(x))

    ir.Register(ir.RAX) -> string_tree.from_string("rax")
    ir.Register(ir.RBX) -> string_tree.from_string("rbx")
    ir.Register(ir.RSP) -> string_tree.from_string("rsp")
    ir.Register(ir.RBP) -> string_tree.from_string("rbp")
    ir.Register(ir.RSI) -> string_tree.from_string("rsi")

    ir.LabelAddress(label) -> string_tree.from_string(label)

    ir.Deref(value:, offset:, multiplier:, size:) -> {
      let ptr = case size {
        0 -> string_tree.new()
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

    ir.Move(to:, from:) -> {
      case to, from {
        ir.Deref(..), ir.Deref(..) -> {
          string_tree.concat([
            string_tree.from_string("push rax\n"),
            string_tree.from_string("mov rax, "),
            serialize_value(from),
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", rax\n"),
            string_tree.from_string("pop rax"),
          ])
        }
        _, _ ->
          string_tree.concat([
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", "),
            serialize_value(from),
          ])
      }
    }

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
