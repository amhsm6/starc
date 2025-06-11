import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result

import starc/codegen/ir
import starc/parser/ast

type Environment {
  Environment(frames: List(Frame), frame_offset: Int)
}

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
  Variable(frame_offset: Int, ty: ir.Type)
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
    types: dict.from_list([#("int", ir.Int), #("bool", ir.Bool)]),
  )
}

fn push_frame(env: Environment) -> Environment {
  let f = Frame(symbols: dict.new(), types: dict.new())
  Environment(..env, frames: [f, ..env.frames])
}

fn pop_frame(env: Environment) -> Environment {
  let assert [_, ..frames] = env.frames
  Environment(..env, frames:)
}

fn set_frame_offset(env: Environment, frame_offset: Int) -> Environment {
  Environment(..env, frame_offset:)
}

fn add_frame_offset(env: Environment, offset: Int) -> Environment {
  Environment(..env, frame_offset: env.frame_offset + offset)
}

fn sub_frame_offset(env: Environment, offset: Int) -> Environment {
  Environment(..env, frame_offset: env.frame_offset - offset)
}

fn insert_symbol(
  env: Environment,
  id: ast.Identifier,
  sym: Symbol,
) -> Environment {
  let assert [frame, ..rest] = env.frames
  let symbols = dict.insert(frame.symbols, id, sym)
  let frame = Frame(..frame, symbols:)
  Environment(..env, frames: [frame, ..rest])
}

fn resolve_type(env: Environment, id: ast.TypeId) -> Result(ir.Type, Error) {
  list.find_map(env.frames, fn(f) { dict.get(f.types, id) })
  |> result.replace_error(UnknownType(id))
}

fn resolve_symbol(env: Environment, id: ast.Identifier) -> Result(Symbol, Error) {
  list.find_map(env.frames, fn(f) { dict.get(f.symbols, id) })
  |> result.replace_error(UnknownSymbol(id))
}

fn analyze_expression(
  env: Environment,
  expr: ast.Expression,
) -> Result(ir.Type, Error) {
  case expr {
    ast.IntExpr(_) -> Ok(ir.Int)
    ast.BoolExpr(_) -> Ok(ir.Bool)
    ast.StringExpr(_) -> todo

    //FIXME?
    ast.VarExpr(id) -> {
      use sym <- result.try(resolve_symbol(env, id))
      case sym {
        Function(..) -> Error(TypeError("Functions cannot be values"))
        Variable(ty:, ..) -> Ok(ty)
      }
    }

    //FIXME
    ast.AddrOfExpr(e) -> {
      case e {
        ast.VarExpr(id) -> {
          use sym <- result.try(resolve_symbol(env, id))
          case sym {
            Variable(ty:, ..) -> Ok(ir.Pointer(ty))
            _ -> Error(TypeError("Can only take address of a variable"))
          }
        }
        _ -> Error(TypeError("Can only take address of a variable"))
      }
    }

    ast.DerefExpr(e) -> {
      use ty <- result.try(analyze_expression(env, e))
      case ty {
        ir.Pointer(ty) -> Ok(ty)
        _ -> Error(TypeError("Can only deref a pointer"))
      }
    }

    //FIXME
    ast.CallExpression(f:, ..) -> {
      let assert ast.VarExpr(id) = f
      use sym <- result.try(resolve_symbol(env, id))
      case sym {
        Function(return:, ..) -> Ok(return)
        _ -> Error(TypeError("Can only call a function"))
      }
    }

    ast.AddExpr(e1, e2)
    | ast.SubExpr(e1, e2)
    | ast.MulExpr(e1, e2)
    | ast.DivExpr(e1, e2) -> {
      use ty1 <- result.try(analyze_expression(env, e1))
      use ty2 <- result.try(analyze_expression(env, e2))
      case ty1, ty2 {
        ir.Void, _ | _, ir.Void ->
          Error(TypeError("Cannot perform math on void"))
        ir.Int, ir.Int -> Ok(ir.Int)
        _, _ -> Error(TypeError("Type mismatch"))
      }
    }

    ast.EQExpr(e1, e2)
    | ast.NEQExpr(e1, e2)
    | ast.GEExpr(e1, e2)
    | ast.GTExpr(e1, e2)
    | ast.LEExpr(e1, e2)
    | ast.LTExpr(e1, e2) -> {
      use ty1 <- result.try(analyze_expression(env, e1))
      use ty2 <- result.try(analyze_expression(env, e2))
      case ty1, ty2 {
        ir.Void, _ | _, ir.Void -> Error(TypeError("Cannot compare void"))
        ir.Bool, ir.Bool -> Ok(ir.Bool)
        _, _ -> Error(TypeError("Type mismatch"))
      }
    }

    ast.NotExpr(e) -> {
      use ty <- result.try(analyze_expression(env, e))
      case ty {
        ir.Void -> Error(TypeError("Cannot invert void"))
        ir.Bool -> Ok(ir.Bool)
        _ -> Error(TypeError("Type mismatch"))
      }
    }
  }
}

fn generate_expression(
  env: Environment,
  expr: ast.Expression,
) -> Result(ir.Value, Error) {
  case expr {
    ast.IntExpr(x) -> Ok(ir.Immediate(x))

    ast.BoolExpr(True) -> Ok(ir.Immediate(1))
    ast.BoolExpr(False) -> Ok(ir.Immediate(0))

    ast.StringExpr(_) -> todo

    ast.VarExpr(id) -> {
      use sym <- result.try(resolve_symbol(env, id))
      case sym {
        Function(..) -> Error(TypeError("Functions cannot be values"))
        Variable(frame_offset:, ..) -> Ok(ir.FrameOffset(frame_offset))
      }
    }

    ast.AddrOfExpr(e) -> {
      case e {
        ast.VarExpr(id) -> {
          use sym <- result.try(resolve_symbol(env, id))
          case sym {
            Variable(frame_offset:, ..) -> Ok(ir.Immediate(frame_offset))
            _ -> Error(TypeError("Can only take address of a variable"))
          }
        }
        _ -> Error(TypeError("Can only take address of a variable"))
      }
    }

    ast.DerefExpr(e) -> {
      use val <- result.try(generate_expression(env, e))
      Ok(case val {
        ir.Address(x) -> {
          todo
        }
        ir.FrameOffset(_) -> todo
        ir.Immediate(x) -> ir.Address(x)
        ir.Register(_) -> todo
      })
    }

    ast.AddExpr(_, _) -> todo
    ast.CallExpression(f:, args:) -> todo
    ast.DivExpr(_, _) -> todo
    ast.EQExpr(_, _) -> todo
    ast.GEExpr(_, _) -> todo
    ast.GTExpr(_, _) -> todo
    ast.LEExpr(_, _) -> todo
    ast.LTExpr(_, _) -> todo
    ast.MulExpr(_, _) -> todo
    ast.NEQExpr(_, _) -> todo
    ast.NotExpr(_) -> todo
    ast.SubExpr(_, _) -> todo
  }
}

fn generate_statement(
  env: Environment,
  statement: ast.Statement,
) -> Result(#(Environment, List(ir.Statement)), Error) {
  case statement {
    ast.DefineStatement(name:, ty: typeid, expr:) -> {
      let assert ast.VarExpr(id) = name

      use expr_ty <- result.try(analyze_expression(env, expr))
      use ty <- result.try(case typeid {
        None -> Ok(expr_ty)
        Some(typeid) -> {
          use ty <- result.try(resolve_type(env, typeid))
          case ty == expr_ty {
            True -> Ok(ty)
            False -> Error(TypeError("Type mismatch"))
          }
        }
      })

      let env = sub_frame_offset(env, ir.size_of(expr_ty))
      let frame_offset = env.frame_offset
      let env = insert_symbol(env, id, Variable(ty:, frame_offset:))

      todo
    }

    ast.AssignStatement(cell:, expr:) -> todo

    ast.CallStatement(_) -> todo

    ast.IfStatement(condition:, block:, elseifs:, elseblock:) -> todo

    ast.ReturnStatement(_) -> todo
  }
  todo
}

fn generate_function(
  env: Environment,
  declaration: ast.Declaration,
) -> Result(ir.Function, Error) {
  let assert ast.FunctionDeclaration(name:, body:, ..) = declaration

  use sym <- result.try(resolve_symbol(env, name))
  let assert Function(label:, args:, return:) = sym

  let env = push_frame(env) |> set_frame_offset(16)

  use env <- result.try(
    list.try_fold(args, env, fn(env, x) {
      let #(id, ty) = x
      case resolve_symbol(env, id) {
        Ok(_) -> Error(DuplicateSymbol(id))
        Error(_) -> {
          Ok(
            insert_symbol(
              env,
              id,
              Variable(frame_offset: env.frame_offset, ty:),
            )
            |> add_frame_offset(ir.size_of(ty)),
          )
        }
      }
    }),
  )

  let env = set_frame_offset(env, 0)

  let #(res, body) =
    list.map_fold(body, Ok(env), fn(prev, statement) {
      case prev {
        Error(..) -> #(prev, [])
        Ok(env) -> {
          case generate_statement(env, statement) {
            Error(err) -> #(Error(err), [])
            Ok(#(env, code)) -> #(Ok(env), code)
          }
        }
      }
    })

  use env <- result.try(res)

  Ok(ir.Function(label:, body: list.flatten(body)))
}

pub fn generate_program(tree: ast.Program) -> Result(ir.Program, Error) {
  let env = Environment(frames: [builtin()], frame_offset: 0)

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

  // TODO: filter out type definitions

  list.try_map(tree, generate_function(env, _))
}
