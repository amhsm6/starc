import gleam/dict.{type Dict}
import gleam/list
import gleam/result

import starc/parser/ast

pub type Environment {
  Environment(frames: List(Frame), frame_offset: Int)
}

pub type Frame {
  Frame(
    symbols: Dict(ast.Identifier, Symbol),
    types: Dict(ast.Identifier, ast.Type),
  )
}

pub type Symbol {
  Function(
    label: String,
    args: List(#(ast.Identifier, ast.Type)),
    return_type: ast.Type,
  )
  Variable(frame_offset: Int, ty: ast.Type)
}

pub type CodegenError {
  UnknownType(ast.Identifier)
  UnknownSymbol(ast.Identifier)
  TypeError(String)
  DuplicateType(ast.Identifier)
  DuplicateSymbol(ast.Identifier)
}

pub fn builtin() -> Frame {
  Frame(
    symbols: dict.from_list([
      #(
        "print_bool",
        Function(
          label: "print_bool",
          args: [#("x", ast.Bool)],
          return_type: ast.Void,
        ),
      ),
      #(
        "print_int64",
        Function(
          label: "print_int64",
          args: [#("x", ast.Int64)],
          return_type: ast.Void,
        ),
      ),
    ]),
    types: dict.from_list([
      #("bool", ast.Bool),
      #("int8", ast.Int8),
      #("int16", ast.Int16),
      #("int32", ast.Int32),
      #("int64", ast.Int64),
    ]),
  )
}

pub fn push_frame(env: Environment) -> Environment {
  let f = Frame(symbols: dict.new(), types: dict.new())
  Environment(..env, frames: [f, ..env.frames])
}

pub fn pop_frame(env: Environment) -> Environment {
  let assert [_, ..frames] = env.frames
  Environment(..env, frames:)
}

// FIXME
pub fn set_frame_offset(env: Environment, frame_offset: Int) -> Environment {
  Environment(..env, frame_offset:)
}

pub fn add_frame_offset(env: Environment, offset: Int) -> Environment {
  Environment(..env, frame_offset: env.frame_offset + offset)
}

pub fn sub_frame_offset(env: Environment, offset: Int) -> Environment {
  Environment(..env, frame_offset: env.frame_offset - offset)
}

pub fn insert_symbol(
  env: Environment,
  id: ast.Identifier,
  sym: Symbol,
) -> Environment {
  let assert [frame, ..rest] = env.frames
  let symbols = dict.insert(frame.symbols, id, sym)
  let frame = Frame(..frame, symbols:)
  Environment(..env, frames: [frame, ..rest])
}

pub fn resolve_type(
  env: Environment,
  id: ast.TypeId,
) -> Result(ast.Type, CodegenError) {
  list.find_map(env.frames, fn(f) { dict.get(f.types, id) })
  |> result.replace_error(UnknownType(id))
}

pub fn resolve_symbol(
  env: Environment,
  id: ast.Identifier,
) -> Result(Symbol, CodegenError) {
  list.find_map(env.frames, fn(f) { dict.get(f.symbols, id) })
  |> result.replace_error(UnknownSymbol(id))
}

pub fn assert_unique_symbol(
  env: Environment,
  id: ast.Identifier,
) -> Result(Nil, CodegenError) {
  case list.find(env.frames, fn(f) { dict.has_key(f.symbols, id) }) {
    Ok(_) -> Error(DuplicateSymbol(id))
    Error(_) -> Ok(Nil)
  }
}

pub fn assert_unique_type(
  env: Environment,
  id: ast.TypeId,
) -> Result(Nil, CodegenError) {
  case list.find(env.frames, fn(f) { dict.has_key(f.types, id) }) {
    Ok(_) -> Error(DuplicateType(id))
    Error(_) -> Ok(Nil)
  }
}
