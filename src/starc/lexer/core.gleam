pub type Lexer(a, r) {
  Lexer(run: fn(LexerState, fn(LexerState, a) -> r, fn() -> r) -> r)
}

pub type LexerState {
  LexerState(input: String, pos: Pos)
}

pub type Pos {
  Pos(line: Int, char: Int)
}

pub fn begin() -> Pos {
  Pos(1, 1)
}

pub fn advance_chars(pos: Pos, n: Int) -> Pos {
  let Pos(line, char) = pos
  Pos(line, char + n)
}

pub fn advance_char(pos: Pos) -> Pos {
  advance_chars(pos, 1)
}

pub fn advance_line(pos: Pos) -> Pos {
  Pos(pos.line + 1, 1)
}

pub fn perform(l: Lexer(a, r), f: fn(a) -> Lexer(b, r)) -> Lexer(b, r) {
  use state, succ, fail <- Lexer
  l.run(state, fn(state, x) { f(x).run(state, succ, fail) }, fail)
}

pub fn pure(value: a) -> Lexer(a, r) {
  Lexer(fn(state, succ, _) { succ(state, value) })
}

pub fn die() -> Lexer(a, r) {
  Lexer(fn(_, _, fail) { fail() })
}

pub fn many(l: Lexer(a, r)) -> Lexer(List(a), r) {
  use state, succ, fail <- Lexer

  l.run(
    state,
    fn(state, x) {
      many(l).run(state, fn(state, xs) { succ(state, [x, ..xs]) }, fail)
    },
    fn() { succ(state, []) },
  )
}

pub fn some(l: Lexer(a, r)) -> Lexer(List(a), r) {
  use xs <- perform(many(l))
  case xs {
    [] -> die()
    xs -> pure(xs)
  }
}
