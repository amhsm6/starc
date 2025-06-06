import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

import starc/parser/ast.{type Identifier}

type Environment =
  List(Frame)

type Frame {
  Frame(symbols: Dict(Identifier, Option(Type)), types: Dict(Identifier, Type))
}

type Type {
  Void
  Int
  Float
  Bool
  String
  Function(args: List(Type), return: Type)
}

pub type Error {
  UnknownType(Identifier)
  UnknownSymbol(Identifier)
  TypeError(String)
  DuplicateType(Identifier)
  DuplicateSymbol(Identifier)
}

fn builtin() -> Frame {
  Frame(
    symbols: dict.from_list([
      #("print", Some(Function(args: [String], return: Void))),
    ]),
    types: dict.from_list([
      #("int", Int),
      #("float", Float),
      #("Bool", Bool),
      #("String", String),
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
  id: Identifier,
  ty: Option(Type),
) -> Environment {
  let assert [f, ..rest] = env
  let symbols = dict.insert(f.symbols, id, ty)
  [Frame(..f, symbols:), ..rest]
}

fn resolve_type(env: Environment, id: Identifier) -> Result(Type, Error) {
  list.find_map(env, fn(f) { dict.get(f.types, id) })
  |> result.replace_error(UnknownType(id))
}

fn resolve_symbol(
  env: Environment,
  id: Identifier,
) -> Result(Option(Type), Error) {
  list.find_map(env, fn(f) { dict.get(f.symbols, id) })
  |> result.replace_error(UnknownSymbol(id))
}

pub fn analyze(tree: ast.Program) -> Result(Nil, Error) {
  let env = [builtin()]

  use env <- result.try(
    list.try_fold(tree, env, fn(env, declaration) {
      case declaration {
        ast.FunctionDeclaration(name:, parameters:, result:, ..) -> {
          case resolve_symbol(env, name) {
            Ok(_) -> Error(DuplicateSymbol(name))
            Error(_) -> {
              use param_types <- result.try(
                list.try_map(parameters, fn(x) {
                  resolve_type(env, pair.second(x))
                }),
              )
              use return_type <- result.try(
                option.map(result, resolve_type(env, _))
                |> option.unwrap(Ok(Void)),
              )
              Ok(insert_symbol(
                env,
                name,
                Some(Function(args: param_types, return: return_type)),
              ))
            }
          }
        }
      }
    }),
  )

  echo env

  todo
}
