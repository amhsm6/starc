import gleam/int
import gleam/list
import gleam/string_tree.{type StringTree}

import starc/codegen/ir

fn header() -> String {
  "
  .intel_syntax noprefix
  .section .text
  .global _start
  _start:
  call main
  mov rax, 60
  mov rdi, 0
  syscall
  ret
  "
}

fn serialize_value(value: ir.Value) -> StringTree {
  case value {
    ir.Immediate(x) -> string_tree.from_string(int.to_string(x))

    ir.Register(ir.RAX) -> string_tree.from_string("rax")
    ir.Register(ir.EAX) -> string_tree.from_string("eax")
    ir.Register(ir.AX) -> string_tree.from_string("ax")
    ir.Register(ir.AH) -> string_tree.from_string("ah")
    ir.Register(ir.AL) -> string_tree.from_string("al")

    ir.Register(ir.RBX) -> string_tree.from_string("rbx")
    ir.Register(ir.EBX) -> string_tree.from_string("ebx")
    ir.Register(ir.BX) -> string_tree.from_string("bx")
    ir.Register(ir.BH) -> string_tree.from_string("bh")
    ir.Register(ir.BL) -> string_tree.from_string("bl")

    ir.Register(ir.RCX) -> string_tree.from_string("rcx")
    ir.Register(ir.ECX) -> string_tree.from_string("ecx")
    ir.Register(ir.CX) -> string_tree.from_string("cx")
    ir.Register(ir.CH) -> string_tree.from_string("ch")
    ir.Register(ir.CL) -> string_tree.from_string("cl")

    ir.Register(ir.RDX) -> string_tree.from_string("rdx")
    ir.Register(ir.EDX) -> string_tree.from_string("edx")
    ir.Register(ir.DX) -> string_tree.from_string("dx")
    ir.Register(ir.DH) -> string_tree.from_string("dh")
    ir.Register(ir.DL) -> string_tree.from_string("dl")

    ir.Register(ir.RDI) -> string_tree.from_string("rdi")
    ir.Register(ir.EDI) -> string_tree.from_string("edi")
    ir.Register(ir.DI) -> string_tree.from_string("di")
    ir.Register(ir.DIL) -> string_tree.from_string("dil")

    ir.Register(ir.RSI) -> string_tree.from_string("rsi")
    ir.Register(ir.ESI) -> string_tree.from_string("esi")
    ir.Register(ir.SI) -> string_tree.from_string("si")
    ir.Register(ir.SIL) -> string_tree.from_string("sil")

    ir.Register(ir.RBP) -> string_tree.from_string("rbp")
    ir.Register(ir.RSP) -> string_tree.from_string("rsp")

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
  }
}

// FIXME: div semantics
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
  |> string_tree.prepend(header())
  |> string_tree.append("\n")
  |> string_tree.to_string()
}
