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
  call exit
  ret
  "
}

fn serialize_register(reg: ir.Value) -> StringTree {
  case reg {
    ir.Register(ir.RegA, size: 8) -> string_tree.from_string("rax")
    ir.Register(ir.RegA, size: 4) -> string_tree.from_string("eax")
    ir.Register(ir.RegA, size: 2) -> string_tree.from_string("ax")
    ir.Register(ir.RegA, size: 1) -> string_tree.from_string("al")
    ir.Register(ir.RegA, size: _) -> panic

    ir.Register(ir.RegB, size: 8) -> string_tree.from_string("rbx")
    ir.Register(ir.RegB, size: 4) -> string_tree.from_string("ebx")
    ir.Register(ir.RegB, size: 2) -> string_tree.from_string("bx")
    ir.Register(ir.RegB, size: 1) -> string_tree.from_string("bl")
    ir.Register(ir.RegB, size: _) -> panic

    ir.Register(ir.RegC, size: 8) -> string_tree.from_string("rcx")
    ir.Register(ir.RegC, size: 4) -> string_tree.from_string("ecx")
    ir.Register(ir.RegC, size: 2) -> string_tree.from_string("cx")
    ir.Register(ir.RegC, size: 1) -> string_tree.from_string("cl")
    ir.Register(ir.RegC, size: _) -> panic

    ir.Register(ir.RegD, size: 8) -> string_tree.from_string("rdx")
    ir.Register(ir.RegD, size: 4) -> string_tree.from_string("edx")
    ir.Register(ir.RegD, size: 2) -> string_tree.from_string("dx")
    ir.Register(ir.RegD, size: 1) -> string_tree.from_string("dl")
    ir.Register(ir.RegD, size: _) -> panic

    _ -> panic
  }
}

fn serialize_value(value: ir.Value) -> StringTree {
  case value {
    ir.Immediate(value:, ..) -> string_tree.from_string(int.to_string(value))

    ir.LabelAddress(label) -> string_tree.from_string(label)

    ir.Register(..) -> serialize_register(value)

    ir.Deref(value:, offset:, multiplier:, size:) -> {
      let ptr = case size {
        0 -> string_tree.new()
        1 -> string_tree.from_string("byte ptr")
        2 -> string_tree.from_string("word ptr")
        4 -> string_tree.from_string("dword ptr")
        8 -> string_tree.from_string("qword ptr")
        _ -> panic
      }

      assert ir.size_of(value) == 8
      assert ir.size_of(offset) == 8

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

    ir.RBP -> string_tree.from_string("rbp")
    ir.RSP -> string_tree.from_string("rsp")
    ir.RSI -> string_tree.from_string("rsi")
  }
}

fn serialize_statement(statement: ir.Statement) -> StringTree {
  case statement {
    ir.Add(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      string_tree.concat([
        string_tree.from_string("add "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])
    }

    ir.Call(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("call "), serialize_value(x)])
    }

    ir.Div(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      let size = ir.size_of(to)

      let out_register = serialize_register(ir.Register(ir.RegA, size:))

      let extension = case size {
        8 -> string_tree.from_string("cqo")
        4 -> string_tree.from_string("cdq")
        2 -> string_tree.from_string("cwd")
        1 -> string_tree.from_string("movsx ax, al")
        _ -> panic
      }

      case from {
        ir.Immediate(..)
        | ir.Register(reg: ir.RegA, ..)
        | ir.Deref(value: ir.Register(reg: ir.RegA, ..), ..) -> {
          let aux_register = serialize_register(ir.Register(ir.RegC, size:))
          string_tree.concat([
            string_tree.from_string("mov "),
            aux_register,
            string_tree.from_string(", "),
            serialize_value(from),
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            out_register,
            string_tree.from_string(", "),
            serialize_value(to),
            string_tree.from_string("\n"),
            extension,
            string_tree.from_string("\n"),
            string_tree.from_string("idiv "),
            aux_register,
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", "),
            out_register,
          ])
        }

        _ ->
          string_tree.concat([
            string_tree.from_string("mov "),
            out_register,
            string_tree.from_string(", "),
            serialize_value(to),
            string_tree.from_string("\n"),
            extension,
            string_tree.from_string("\n"),
            string_tree.from_string("idiv "),
            serialize_value(from),
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", "),
            out_register,
          ])
      }
    }

    ir.Epilogue ->
      string_tree.from_strings(["mov rsp, rbp\n", "pop rbp\n", "ret"])

    ir.Label(x) -> string_tree.from_string(x <> ":")

    ir.Lea(to:, from:) -> {
      assert ir.size_of(to) == 8
      string_tree.concat([
        string_tree.from_string("lea "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])
    }

    ir.Move(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      let size = ir.size_of(to)

      case to, from {
        ir.Deref(..), ir.Deref(..) -> {
          let aux_register = ir.Register(ir.RegC, size:)
          string_tree.concat([
            string_tree.from_string("mov "),
            serialize_register(aux_register),
            string_tree.from_string(", "),
            serialize_value(from),
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", "),
            serialize_register(aux_register),
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

    ir.Mul(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      let size = ir.size_of(to)

      let out_register = serialize_register(ir.Register(ir.RegA, size:))

      case from {
        ir.Immediate(..)
        | ir.Register(reg: ir.RegA, ..)
        | ir.Deref(value: ir.Register(reg: ir.RegA, ..), ..) -> {
          let aux_register = serialize_register(ir.Register(ir.RegC, size:))
          string_tree.concat([
            string_tree.from_string("mov "),
            aux_register,
            string_tree.from_string(", "),
            serialize_value(from),
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            out_register,
            string_tree.from_string(", "),
            serialize_value(to),
            string_tree.from_string("\n"),
            string_tree.from_string("imul "),
            aux_register,
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", "),
            out_register,
          ])
        }

        _ ->
          string_tree.concat([
            string_tree.from_string("mov "),
            out_register,
            string_tree.from_string(", "),
            serialize_value(to),
            string_tree.from_string("\n"),
            string_tree.from_string("imul "),
            serialize_value(from),
            string_tree.from_string("\n"),
            string_tree.from_string("mov "),
            serialize_value(to),
            string_tree.from_string(", "),
            out_register,
          ])
      }
    }

    ir.Pop(x) -> {
      case ir.size_of(x) {
        8 | 2 ->
          string_tree.concat([
            string_tree.from_string("pop "),
            serialize_value(x),
          ])

        4 as size | 1 as size ->
          string_tree.concat([
            serialize_statement(ir.Move(to: x, from: ir.deref(ir.RSP, 0, size))),
            string_tree.from_string("\n"),
            serialize_statement(ir.Add(
              to: ir.RSP,
              from: ir.Immediate(value: size, size: 8),
            )),
          ])

        _ -> panic
      }
    }

    ir.Prologue(reserve_bytes:) ->
      string_tree.from_strings([
        "push rbp\n",
        "mov rbp, rsp\n",
        "sub rsp, ",
        int.to_string(reserve_bytes),
      ])

    ir.Push(x) -> {
      case ir.size_of(x) {
        8 | 2 ->
          string_tree.concat([
            string_tree.from_string("push "),
            serialize_value(x),
          ])

        4 as size | 1 as size ->
          string_tree.concat([
            serialize_statement(ir.Sub(
              to: ir.RSP,
              from: ir.Immediate(value: size, size: 8),
            )),
            string_tree.from_string("\n"),
            serialize_statement(ir.Move(to: ir.deref(ir.RSP, 0, size), from: x)),
          ])

        _ -> panic
      }
    }

    ir.Sub(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      string_tree.concat([
        string_tree.from_string("sub "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])
    }

    ir.Cmp(x, y) -> {
      assert ir.size_of(x) == ir.size_of(y)

      case x {
        ir.Immediate(size:, ..) -> {
          let aux_register =
            serialize_register(ir.Register(reg: ir.RegC, size:))
          string_tree.concat([
            string_tree.from_string("mov "),
            aux_register,
            string_tree.from_string(", "),
            serialize_value(x),
            string_tree.from_string("\n"),
            string_tree.from_string("cmp "),
            aux_register,
            string_tree.from_string(", "),
            serialize_value(y),
          ])
        }

        _ ->
          string_tree.concat([
            string_tree.from_string("cmp "),
            serialize_value(x),
            string_tree.from_string(", "),
            serialize_value(y),
          ])
      }
    }

    ir.ExtractZF(to) -> {
      let size = ir.size_of(to)
      let aux_register = serialize_register(ir.Register(reg: ir.RegC, size:))

      string_tree.concat([
        string_tree.from_string("pushf\n"),
        string_tree.from_string("mov rcx, 0b0000000100000110\n"),
        string_tree.from_string("bextr rcx, qword ptr [rsp], rcx\n"),
        string_tree.from_string("mov "),
        serialize_value(to),
        string_tree.from_string(", "),
        aux_register,
        string_tree.from_string("\n"),
        string_tree.from_string("add rsp, 8"),
      ])
    }

    ir.And(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      string_tree.concat([
        string_tree.from_string("and "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])
    }

    ir.Or(to:, from:) -> {
      assert ir.size_of(to) == ir.size_of(from)
      string_tree.concat([
        string_tree.from_string("or "),
        serialize_value(to),
        string_tree.from_string(", "),
        serialize_value(from),
      ])
    }

    ir.Not(x) ->
      string_tree.concat([string_tree.from_string("not "), serialize_value(x)])

    ir.Neg(x) ->
      string_tree.concat([string_tree.from_string("neg "), serialize_value(x)])

    ir.JE(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("je "), serialize_value(x)])
    }

    ir.JNE(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("jne "), serialize_value(x)])
    }

    ir.JGE(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("jge "), serialize_value(x)])
    }

    ir.JGT(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("jg "), serialize_value(x)])
    }

    ir.JLE(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("jle "), serialize_value(x)])
    }

    ir.JLT(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("jl "), serialize_value(x)])
    }

    ir.Jump(x) -> {
      assert ir.size_of(x) == 8
      string_tree.concat([string_tree.from_string("jmp "), serialize_value(x)])
    }
  }
}

pub fn serialize_program(code: ir.Program) -> String {
  list.map(code, serialize_statement)
  |> string_tree.join("\n")
  |> string_tree.prepend(header())
  |> string_tree.append("\n")
  |> string_tree.to_string()
}
