import gleam/option.{None, Some}

import starc/codegen/core.{
  type Generator, add_frame_offset, assert_unique_symbol, emit, generate,
  get_frame_offset, insert_symbol, perform, pop_frame, pure, push_frame,
  resolve_symbol, resolve_type, set_frame_offset, traverse,
}
import starc/codegen/env.{Function, Variable}
import starc/parser/ast

//fn analyze_expression(
//   env: Environment,
//   expr: ast.Expression,
// ) -> Result(ir.Type, Error) {
//   case expr {
//     ast.IntExpr(_) -> Ok(ir.Int)
//     ast.BoolExpr(_) -> Ok(ir.Bool)
//     ast.StringExpr(_) -> todo

//     //FIXME?
//     ast.VarExpr(id) -> {
//       use sym <- result.try(resolve_symbol(env, id))
//       case sym {
//         Function(..) -> Error(TypeError("Functions cannot be values"))
//         Variable(ty:, ..) -> Ok(ty)
//       }
//     }

//     //FIXME
//     ast.AddrOfExpr(e) -> {
//       case e {
//         ast.VarExpr(id) -> {
//           use sym <- result.try(resolve_symbol(env, id))
//           case sym {
//             Variable(ty:, ..) -> Ok(ir.Pointer(ty))
//             _ -> Error(TypeError("Can only take address of a variable"))
//           }
//         }
//         _ -> Error(TypeError("Can only take address of a variable"))
//       }
//     }

//     ast.DerefExpr(e) -> {
//       use ty <- result.try(analyze_expression(env, e))
//       case ty {
//         ir.Pointer(ty) -> Ok(ty)
//         _ -> Error(TypeError("Can only deref a pointer"))
//       }
//     }

//     //FIXME
//     ast.CallExpression(f:, ..) -> {
//       let assert ast.VarExpr(id) = f
//       use sym <- result.try(resolve_symbol(env, id))
//       case sym {
//         Function(return:, ..) -> Ok(return)
//         _ -> Error(TypeError("Can only call a function"))
//       }
//     }

//     ast.AddExpr(e1, e2)
//     | ast.SubExpr(e1, e2)
//     | ast.MulExpr(e1, e2)
//     | ast.DivExpr(e1, e2) -> {
//       use ty1 <- result.try(analyze_expression(env, e1))
//       use ty2 <- result.try(analyze_expression(env, e2))
//       case ty1, ty2 {
//         ir.Void, _ | _, ir.Void ->
//           Error(TypeError("Cannot perform math on void"))
//         ir.Int, ir.Int -> Ok(ir.Int)
//         _, _ -> Error(TypeError("Type mismatch"))
//       }
//     }

//     ast.EQExpr(e1, e2)
//     | ast.NEQExpr(e1, e2)
//     | ast.GEExpr(e1, e2)
//     | ast.GTExpr(e1, e2)
//     | ast.LEExpr(e1, e2)
//     | ast.LTExpr(e1, e2) -> {
//       use ty1 <- result.try(analyze_expression(env, e1))
//       use ty2 <- result.try(analyze_expression(env, e2))
//       case ty1, ty2 {
//         ir.Void, _ | _, ir.Void -> Error(TypeError("Cannot compare void"))
//         ir.Bool, ir.Bool -> Ok(ir.Bool)
//         _, _ -> Error(TypeError("Type mismatch"))
//       }
//     }

//     ast.NotExpr(e) -> {
//       use ty <- result.try(analyze_expression(env, e))
//       case ty {
//         ir.Void -> Error(TypeError("Cannot invert void"))
//         ir.Bool -> Ok(ir.Bool)
//         _ -> Error(TypeError("Type mismatch"))
//       }
//     }
//   }
// }

fn analyze_statement(statement: ast.Statement) -> Generator(ast.Statement, r) {
  let assert ast.UntypedStatement(statement) = statement
  case statement {
    ast.UntypedAssignStatement(cell:, expr:) -> todo
    ast.UntypedCallStatement(_) -> todo
    ast.UntypedDefineStatement(name:, typeid:, expr:) -> todo
    ast.UntypedIfStatement(condition:, block:, elseifs:, elseblock:) -> todo
    ast.UntypedReturnStatement(_) -> todo
  }
}

// FIXME: declaration only sees items defined above
fn analyze_declaration(
  declaration: ast.Declaration,
) -> Generator(ast.Declaration, r) {
  let assert ast.UntypedDeclaration(declaration) = declaration
  case declaration {
    ast.UntypedFunctionDeclaration(name:, parameters:, result:, body:) -> {
      use _ <- perform(assert_unique_symbol(name))

      use args <- perform(
        traverse(parameters, fn(x) {
          let #(id, typeid) = x
          use ty <- perform(resolve_type(typeid))
          pure(#(id, ty))
        }),
      )

      use return_type <- perform(case result {
        None -> pure(ast.Void)
        Some(typeid) -> resolve_type(typeid)
      })

      use _ <- perform(insert_symbol(
        name,
        Function(label: name, args:, return_type:),
      ))

      use _ <- perform(push_frame())

      use _ <- perform(
        traverse(args, fn(x) {
          let #(id, ty) = x
          use _ <- perform(assert_unique_symbol(id))
          insert_symbol(id, Variable(frame_offset: 0, ty:))
        }),
      )

      use _ <- perform(set_frame_offset(0))
      use body <- perform(traverse(body, analyze_statement))
      use frame_offset <- perform(get_frame_offset())

      use _ <- perform(pop_frame())

      pure(
        ast.TypedDeclaration(ast.TypedFunctionDeclaration(
          name:,
          parameters: args,
          result: return_type,
          body:,
          reserve_bytes: -frame_offset,
        )),
      )
    }
  }
}

pub fn analyze_program(tree: ast.Program) -> Generator(ast.Program, r) {
  traverse(tree, analyze_declaration)
}
