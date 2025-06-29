import gleam/list

import starc/codegen/env.{
  type Environment, type SemanticError, type Symbol, Environment,
}
import starc/codegen/ir
import starc/parser/ast

pub type Generator(a, r) {
  Generator(
    run: fn(
      Environment,
      fn(Environment, List(ir.Statement), a) -> r,
      fn(Environment, a) -> r,
      fn(SemanticError) -> r,
    ) ->
      r,
  )
}

pub fn pure(x: a) -> Generator(a, r) {
  use env, succ, _, _ <- Generator
  succ(env, [], x)
}

pub fn die(err: SemanticError) -> Generator(a, r) {
  use _, _, _, fail <- Generator
  fail(err)
}

pub fn return_found(x: a) -> Generator(a, r) {
  use env, _, return_found, _ <- Generator
  return_found(env, x)
}

pub fn perform(
  g: Generator(a, r),
  f: fn(a) -> Generator(b, r),
) -> Generator(b, r) {
  use env, succ, return_found, fail <- Generator

  g.run(
    env,
    fn(env, code, x) {
      f(x).run(
        env,
        fn(env, code2, y) { succ(env, list.append(code, code2), y) },
        return_found,
        fail,
      )
    },
    fn(_, _) { panic },
    fail,
  )
}

pub fn map(g: Generator(a, r), f: fn(a) -> b) -> Generator(b, r) {
  perform(g, fn(x) { pure(f(x)) })
}

pub fn emit(code: List(ir.Statement)) -> Generator(Nil, r) {
  use env, succ, _, _ <- Generator
  succ(env, code, Nil)
}

pub fn get() -> Generator(Environment, r) {
  use env, succ, _, _ <- Generator
  succ(env, [], env)
}

pub fn set(env: Environment) -> Generator(Nil, r) {
  use _, succ, _, _ <- Generator
  succ(env, [], Nil)
}

pub fn traverse(
  list: List(a),
  f: fn(a) -> Generator(b, r),
) -> Generator(List(b), r) {
  list.fold(list, pure([]), fn(gen, element) {
    use x <- perform(gen)
    use y <- perform(f(element))
    pure([y, ..x])
  })
  |> map(list.reverse)
}

pub fn traverse_until_return(
  list: List(a),
  f: fn(a) -> Generator(b, Result(#(Environment, List(b), Bool), SemanticError)),
) -> Generator(#(List(b), Bool), r) {
  use env, succ, _, fail <- Generator

  case traverse_until_return_loop(list, f, env, []) {
    Ok(#(env, x, return_found)) -> succ(env, [], #(x, return_found))
    Error(err) -> fail(err)
  }
}

fn traverse_until_return_loop(
  list: List(a),
  f: fn(a) -> Generator(b, Result(#(Environment, List(b), Bool), SemanticError)),
  env: Environment,
  result: List(b),
) -> Result(#(Environment, List(b), Bool), SemanticError) {
  case list {
    [x, ..xs] ->
      f(x).run(
        env,
        fn(env, _, y) { traverse_until_return_loop(xs, f, env, [y, ..result]) },
        fn(env, y) { Ok(#(env, list.reverse([y, ..result]), True)) },
        Error,
      )

    [] -> Ok(#(env, list.reverse(result), False))
  }
}

pub fn generate(
  gen: Generator(a, Result(List(ir.Statement), SemanticError)),
) -> Result(List(ir.Statement), SemanticError) {
  let env =
    Environment(
      frames: [env.builtin()],
      frame_offset: 0,
      return_type: ast.Void,
      label_counter: 0,
    )

  gen.run(env, fn(_, code, _) { Ok(code) }, fn(_, _) { panic }, Error)
}

pub fn push_frame() -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.push_frame(env))
}

pub fn pop_frame() -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.pop_frame(env))
}

pub fn set_frame_offset(frame_offset: Int) -> Generator(Nil, r) {
  use env <- perform(get())
  set(Environment(..env, frame_offset:))
}

pub fn get_frame_offset() -> Generator(Int, r) {
  use env <- perform(get())
  pure(env.frame_offset)
}

pub fn add_frame_offset(offset: Int) -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.add_frame_offset(env, offset))
}

pub fn sub_frame_offset(offset: Int) -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.sub_frame_offset(env, offset))
}

pub fn set_return_type(ty: ast.Type) -> Generator(Nil, r) {
  use env <- perform(get())
  set(Environment(..env, return_type: ty))
}

pub fn get_return_type() -> Generator(ast.Type, r) {
  use env <- perform(get())
  pure(env.return_type)
}

pub fn generate_label() -> Generator(String, r) {
  use env <- perform(get())
  let #(env, label) = env.generate_label(env)

  use _ <- perform(set(env))
  pure(label)
}

pub fn insert_symbol(id: ast.Identifier, sym: Symbol) -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.insert_symbol(env, id, sym))
}

pub fn resolve_type(typeid: ast.TypeIdentifier) -> Generator(ast.Type, r) {
  use env <- perform(get())
  case env.resolve_type(env, typeid) {
    Ok(ty) -> pure(ty)
    Error(err) -> die(err)
  }
}

pub fn resolve_symbol(id: ast.Identifier) -> Generator(Symbol, r) {
  use env <- perform(get())
  case env.resolve_symbol(env, id) {
    Ok(ty) -> pure(ty)
    Error(err) -> die(err)
  }
}

pub fn assert_unique_symbol(id: ast.Identifier) -> Generator(Nil, r) {
  use env <- perform(get())
  case env.assert_unique_symbol(env, id) {
    Ok(_) -> pure(Nil)
    Error(err) -> die(err)
  }
}

pub fn assert_unique_type(typeid: ast.TypeIdentifier) -> Generator(Nil, r) {
  use env <- perform(get())
  case env.assert_unique_type(env, typeid) {
    Ok(_) -> pure(Nil)
    Error(err) -> die(err)
  }
}
