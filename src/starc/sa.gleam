import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result

import starc/parser/ast
import starc/sa/ir

type Environment =
  List(Frame)

type Frame {
  Frame(
    symbols: Dict(ast.Identifier, Option(ir.Type)),
    types: Dict(ast.Identifier, ir.Type),
  )
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
      #("print_bool", Some(ir.Function(args: [ir.Bool], return: ir.Void))),
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
  ty: Option(ir.Type),
) -> Environment {
  let assert [f, ..rest] = env
  let symbols = dict.insert(f.symbols, id, ty)
  [Frame(..f, symbols:), ..rest]
}

fn resolve_type(env: Environment, id: ast.TypeId) -> Result(ir.Type, Error) {
  list.find_map(env, fn(f) { dict.get(f.types, id) })
  |> result.replace_error(UnknownType(id))
}

fn resolve_symbol(
  env: Environment,
  id: ast.Identifier,
) -> Result(Option(ir.Type), Error) {
  list.find_map(env, fn(f) { dict.get(f.symbols, id) })
  |> result.replace_error(UnknownSymbol(id))
}

pub fn analyze(tree: ast.Program) -> Result(ir.Program, Error) {
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
                |> option.unwrap(Ok(ir.Void)),
              )
              Ok(insert_symbol(
                env,
                name,
                Some(ir.Function(args: param_types, return: return_type)),
              ))
            }
          }
        }
      }
    }),
  )

  use _ <- result.try(
    list.try_map(tree, fn(declaration) {
      use ty <- result.try(resolve_symbol(env, declaration.name))
      let assert Some(ir.Function(args:, return:)) = ty

      let env = push_frame(env)

      use env <- result.try(
        list.try_fold(
          list.zip(list.map(declaration.parameters, pair.first), args),
          env,
          fn(env, x) {
            let #(id, ty) = x
            case resolve_symbol(env, id) {
              Ok(_) -> Error(DuplicateSymbol(id))
              Error(_) -> Ok(insert_symbol(env, id, Some(ty)))
            }
          },
        ),
      )

      echo env

      let env = pop_frame(env)
      todo
    }),
  )

  todo
}
