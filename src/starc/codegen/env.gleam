import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result

import starc/lexer/token.{type Span}
import starc/parser/ast

pub type Environment {
  Environment(
    frames: List(Frame),
    frame_offset: Int,
    return_type: ast.Type,
    label_counter: Int,
  )
}

pub type Frame {
  Frame(symbols: Dict(String, Symbol), types: Dict(String, ast.Type))
}

pub type Symbol {
  Function(label: String, arg_types: List(ast.Type), return_type: ast.Type)
  Variable(frame_offset: Int, ty: ast.Type)
}

pub type SemanticError {
  UnknownType(ast.TypeIdentifier)
  UnknownSymbol(ast.Identifier)
  DuplicateType(ast.TypeIdentifier)
  DuplicateSymbol(ast.Identifier)
  TypeMismatch(ty1: ast.Type, span1: Span, ty2: ast.Type, span2: Span)
  NotInteger(ty: ast.Type, span: Span)
  FunctionAsValue(ast.Identifier)
  AddressNotOfVariable(Span)
  DerefNotPointer(ty: ast.Type, span: Span)
  WrongCallExpression(Span)
  CallNotFunction(Span)
  CallArgumentCountMismatch(expected: Int, actual: Int, span: Span)
  MissingReturn(ty: ast.Type, span: Span)
}

pub fn builtin() -> Frame {
  Frame(
    symbols: dict.from_list([
      #(
        "print_bool",
        Function(
          label: "print_bool",
          arg_types: [ast.Bool],
          return_type: ast.Void,
        ),
      ),
      #(
        "print_int64",
        Function(
          label: "print_int64",
          arg_types: [ast.Int64],
          return_type: ast.Void,
        ),
      ),
      #(
        "print_int32",
        Function(
          label: "print_int32",
          arg_types: [ast.Int32],
          return_type: ast.Void,
        ),
      ),
      #(
        "print_int16",
        Function(
          label: "print_int16",
          arg_types: [ast.Int16],
          return_type: ast.Void,
        ),
      ),
      #(
        "print_int8",
        Function(
          label: "print_int8",
          arg_types: [ast.Int8],
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

pub fn add_frame_offset(env: Environment, offset: Int) -> Environment {
  Environment(..env, frame_offset: env.frame_offset + offset)
}

pub fn sub_frame_offset(env: Environment, offset: Int) -> Environment {
  Environment(..env, frame_offset: env.frame_offset - offset)
}

pub fn generate_label(env: Environment) -> #(Environment, String) {
  let counter = env.label_counter
  #(
    Environment(..env, label_counter: counter + 1),
    "label" <> int.to_string(counter),
  )
}

pub fn insert_symbol(
  env: Environment,
  id: ast.Identifier,
  sym: Symbol,
) -> Environment {
  let assert [frame, ..rest] = env.frames
  let symbols = dict.insert(frame.symbols, id.name, sym)
  let frame = Frame(..frame, symbols:)
  Environment(..env, frames: [frame, ..rest])
}

pub fn resolve_type(
  env: Environment,
  typeid: ast.TypeIdentifier,
) -> Result(ast.Type, SemanticError) {
  let resolve_type_name = fn(typeid) {
    let assert ast.TypeName(name:, ..) = typeid

    list.find_map(env.frames, fn(f) { dict.get(f.types, name) })
    |> result.replace_error(UnknownType(typeid))
  }

  case typeid {
    ast.TypeName(..) -> resolve_type_name(typeid)
    ast.TypePointer(typeid, ..) ->
      resolve_type(env, typeid) |> result.map(ast.Pointer)
  }
}

pub fn resolve_symbol(
  env: Environment,
  id: ast.Identifier,
) -> Result(Symbol, SemanticError) {
  list.find_map(env.frames, fn(f) { dict.get(f.symbols, id.name) })
  |> result.replace_error(UnknownSymbol(id))
}

pub fn assert_unique_symbol(
  env: Environment,
  id: ast.Identifier,
) -> Result(Nil, SemanticError) {
  case list.find(env.frames, fn(f) { dict.has_key(f.symbols, id.name) }) {
    Ok(_) -> Error(DuplicateSymbol(id))
    Error(_) -> Ok(Nil)
  }
}

pub fn assert_unique_type(
  env: Environment,
  typeid: ast.TypeIdentifier,
) -> Result(Nil, SemanticError) {
  let assert_unique_type_name = fn(typeid) {
    let assert ast.TypeName(name:, ..) = typeid

    case list.find(env.frames, fn(f) { dict.has_key(f.types, name) }) {
      Ok(_) -> Error(DuplicateType(typeid))
      Error(_) -> Ok(Nil)
    }
  }

  case typeid {
    ast.TypeName(..) -> assert_unique_type_name(typeid)
    ast.TypePointer(typeid, ..) -> assert_unique_type(env, typeid)
  }
}
