import starc/analyzer
import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, emit, generate,
  get_frame_offset, insert_symbol, perform, pop_frame, pure, push_frame,
  resolve_symbol, resolve_type, set_frame_offset, traverse,
}
import starc/codegen/env.{type CodegenError, Function, Variable}
import starc/codegen/ir
import starc/parser/ast

// 

// fn generate_expression(
//   env: Environment,
//   expr: ast.Expression,
// ) -> Result(ir.Value, Error) {
//   case expr {
//     ast.IntExpr(x) -> Ok(ir.Immediate(x))

//     ast.BoolExpr(True) -> Ok(ir.Immediate(1))
//     ast.BoolExpr(False) -> Ok(ir.Immediate(0))

//     ast.StringExpr(_) -> todo

//     ast.VarExpr(id) -> {
//       use sym <- result.try(resolve_symbol(env, id))
//       case sym {
//         Function(..) -> Error(TypeError("Functions cannot be values"))
//         Variable(frame_offset:, ..) -> Ok(ir.FrameOffset(frame_offset))
//       }
//     }

//     ast.AddrOfExpr(e) -> {
//       case e {
//         ast.VarExpr(id) -> {
//           use sym <- result.try(resolve_symbol(env, id))
//           case sym {
//             Variable(frame_offset:, ..) -> Ok(ir.Immediate(frame_offset))
//             _ -> Error(TypeError("Can only take address of a variable"))
//           }
//         }
//         _ -> Error(TypeError("Can only take address of a variable"))
//       }
//     }

//     ast.DerefExpr(e) -> {
//       use val <- result.try(generate_expression(env, e))
//       Ok(case val {
//         ir.Address(x) -> {
//           todo
//         }
//         ir.FrameOffset(_) -> todo
//         ir.Immediate(x) -> ir.Address(x)
//         ir.Register(_) -> todo
//       })
//     }

//     ast.AddExpr(_, _) -> todo
//     ast.CallExpression(f:, args:) -> todo
//     ast.DivExpr(_, _) -> todo
//     ast.EQExpr(_, _) -> todo
//     ast.GEExpr(_, _) -> todo
//     ast.GTExpr(_, _) -> todo
//     ast.LEExpr(_, _) -> todo
//     ast.LTExpr(_, _) -> todo
//     ast.MulExpr(_, _) -> todo
//     ast.NEQExpr(_, _) -> todo
//     ast.NotExpr(_) -> todo
//     ast.SubExpr(_, _) -> todo
//   }
// }

fn generate_statement(statement: ast.Statement) -> Generator(Nil, r) {
  emit([ir.Prologue(1)])
}

// fn generate_statement(
//   env: Environment,
//   statement: ast.Statement,
// ) -> Result(#(Environment, List(ir.Statement)), Error) {
//   case statement {
//     ast.DefineStatement(name:, ty: typeid, expr:) -> {
//       let assert ast.VarExpr(id) = name

//       use expr_ty <- result.try(analyze_expression(env, expr))
//       use ty <- result.try(case typeid {
//         None -> Ok(expr_ty)
//         Some(typeid) -> {
//           use ty <- result.try(resolve_type(env, typeid))
//           case ty == expr_ty {
//             True -> Ok(ty)
//             False -> Error(TypeError("Type mismatch"))
//           }
//         }
//       })

//       let env = sub_frame_offset(env, ir.size_of(expr_ty))
//       let frame_offset = env.frame_offset
//       let env = insert_symbol(env, id, Variable(ty:, frame_offset:))

//       todo
//     }

//     ast.AssignStatement(cell:, expr:) -> todo

//     ast.CallStatement(_) -> todo

//     ast.IfStatement(condition:, block:, elseifs:, elseblock:) -> todo

//     ast.ReturnStatement(_) -> todo
//   }
//   todo
// }

// fn generate_function(
//   env: Environment,
//   declaration: ast.Declaration,
// ) -> Result(ir.Function, Error) {

//   let env = set_frame_offset(env, 0)

//   let #(res, body) =
//     list.map_fold(body, Ok(env), fn(prev, statement) {
//       case prev {
//         Error(..) -> #(prev, [])
//         Ok(env) -> {
//           case generate_statement(env, statement) {
//             Error(err) -> #(Error(err), [])
//             Ok(#(env, code)) -> #(Ok(env), code)
//           }
//         }
//       }
//     })

//   use env <- result.try(res)

//   Ok(ir.Function(label:, body: list.flatten(body)))
// }

fn generate_function(declaration: ast.Declaration) -> Generator(Nil, r) {
  let assert ast.TypedDeclaration(ast.TypedFunctionDeclaration(
    name: label,
    body:,
    parameters: args,
    result: return_type,
    reserve_bytes:,
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

  use _ <- perform(set_frame_offset(0))
  use _ <- perform(traverse(body, generate_statement))

  use _ <- perform(emit([ir.Epilogue(clear_bytes: reserve_bytes)]))

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
