import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

import starc/codegen/ir
import starc/parser/ast

type Environment =
  List(Frame)

type Frame {
  Frame(
    symbols: Dict(ast.Identifier, Symbol),
    types: Dict(ast.Identifier, ir.Type),
  )
}

type Symbol {
  Function(
    label: String,
    args: List(#(ast.Identifier, ir.Type)),
    return: ir.Type,
  )
  Variable(address: Int, ty: Option(ir.Type))
}

pub type Error {
  UnknownType(ast.Identifier)
  UnknownSymbol(ast.Identifier)
  TypeError(String)
  DuplicateType(ast.Identifier)
  DuplicateSymbol(ast.Identifier)
}

fn builtin() -> Frame {
  Frame(
    symbols: dict.from_list([
      #(
        "print_bool",
        Function(label: "print_bool", args: [#("x", ir.Bool)], return: ir.Void),
      ),
    ]),
    types: dict.from_list([
      #("int", ir.Int),
      #("float", ir.Float),
      #("Bool", ir.Bool),
    ]),
  )
}

fn push_frame(env: Environment) -> Environment {
  let f = Frame(symbols: dict.new(), types: dict.new())
  [f, ..env]
}

fn pop_frame(env: Environment) -> Environment {
  let assert [_, ..env] = env
  env
}

fn insert_symbol(
  env: Environment,
  id: ast.Identifier,
  ty: Symbol,
) -> Environment {
  let assert [f, ..rest] = env
  let symbols = dict.insert(f.symbols, id, ty)
  [Frame(..f, symbols:), ..rest]
}

fn resolve_type(env: Environment, id: ast.TypeId) -> Result(ir.Type, Error) {
  list.find_map(env, fn(f) { dict.get(f.types, id) })
  |> result.replace_error(UnknownType(id))
}

fn resolve_symbol(env: Environment, id: ast.Identifier) -> Result(Symbol, Error) {
  list.find_map(env, fn(f) { dict.get(f.symbols, id) })
  |> result.replace_error(UnknownSymbol(id))
}

fn generate_function(
  env: Environment,
  declaration: ast.Declaration,
) -> Result(ir.Function, Error) {
  let assert ast.FunctionDeclaration(name:, body:, ..) = declaration

  use sym <- result.try(resolve_symbol(env, name))
  let assert Function(label:, args:, return:) = sym

  let env = push_frame(env)

  use env <- result.try(
    list.try_fold(args, env, fn(env, x) {
      let #(id, ty) = x
      case resolve_symbol(env, id) {
        Ok(_) -> Error(DuplicateSymbol(id))
        Error(_) -> Ok(insert_symbol(env, id, Variable(0, Some(ty))))
      }
    }),
  )

  echo env
  todo
}

pub fn generate_program(tree: ast.Program) -> Result(ir.Program, Error) {
  let env = [builtin()]

  use env <- result.try(
    list.try_fold(tree, env, fn(env, declaration) {
      case declaration {
        ast.FunctionDeclaration(name:, parameters:, result:, ..) -> {
          case resolve_symbol(env, name) {
            Ok(_) -> Error(DuplicateSymbol(name))
            Error(_) -> {
              use args <- result.try(
                list.try_map(parameters, fn(x) {
                  let #(id, typeid) = x
                  use ty <- result.try(resolve_type(env, typeid))
                  Ok(#(id, ty))
                }),
              )

              use return <- result.try(
                option.map(result, resolve_type(env, _))
                |> option.unwrap(Ok(ir.Void)),
              )

              Ok(insert_symbol(env, name, Function(label: name, args:, return:)))
            }
          }
        }
      }
    }),
  )

  list.try_map(tree, generate_function(env, _))
}
