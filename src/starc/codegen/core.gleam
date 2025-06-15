import gleam/list

import starc/codegen/env.{
  type CodegenError, type Environment, type Symbol, Environment,
}
import starc/codegen/ir
import starc/parser/ast

pub type Generator(a, r) {
  Generator(
    run: fn(
      Environment,
      fn(Environment, List(ir.Statement), a) -> r,
      fn(CodegenError) -> r,
    ) ->
      r,
  )
}

pub fn pure(x: a) -> Generator(a, r) {
  use env, succ, _ <- Generator
  succ(env, [], x)
}

pub fn die(err: CodegenError) -> Generator(a, r) {
  use _, _, fail <- Generator
  fail(err)
}

pub fn perform(
  g: Generator(a, r),
  f: fn(a) -> Generator(b, r),
) -> Generator(b, r) {
  use env, succ, fail <- Generator

  g.run(
    env,
    fn(env, code, x) {
      f(x).run(
        env,
        fn(env, code2, y) { succ(env, list.append(code, code2), y) },
        fail,
      )
    },
    fail,
  )
}

pub fn map(g: Generator(a, r), f: fn(a) -> b) -> Generator(b, r) {
  perform(g, fn(x) { pure(f(x)) })
}

pub fn emit(code: List(ir.Statement)) -> Generator(Nil, r) {
  use env, succ, _ <- Generator
  succ(env, code, Nil)
}

pub fn get() -> Generator(Environment, r) {
  use env, succ, _ <- Generator
  succ(env, [], env)
}

pub fn set(env: Environment) -> Generator(Nil, r) {
  use _, succ, _ <- Generator
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

pub fn generate(
  gen: Generator(a, Result(List(ir.Statement), CodegenError)),
) -> Result(List(ir.Statement), CodegenError) {
  let env = Environment(frames: [env.builtin()], frame_offset: 0)
  gen.run(env, fn(_, code, _) { Ok(code) }, Error)
}

pub fn push_frame() -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.push_frame(env))
}

pub fn pop_frame() -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.pop_frame(env))
}

// FIXME
pub fn set_frame_offset(frame_offset: Int) -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.set_frame_offset(env, frame_offset))
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

pub fn insert_symbol(id: ast.Identifier, sym: Symbol) -> Generator(Nil, r) {
  use env <- perform(get())
  set(env.insert_symbol(env, id, sym))
}

pub fn resolve_type(id: ast.TypeId) -> Generator(ast.Type, r) {
  use env <- perform(get())
  case env.resolve_type(env, id) {
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

pub fn assert_unique_type(id: ast.TypeId) -> Generator(Nil, r) {
  use env <- perform(get())
  case env.assert_unique_type(env, id) {
    Ok(_) -> pure(Nil)
    Error(err) -> die(err)
  }
}
