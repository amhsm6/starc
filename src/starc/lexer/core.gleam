import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

import starc/lexer/token.{
  type Pos, type Span, Pos, Span, advance_chars, advance_lines
}

pub type Lexer(a, r) {
  Lexer(run: fn(LexerState, fn(LexerState, Span, a) -> r, fn() -> r) -> r)
}

pub type LexerState {
  LexerState(input: String, pos: Pos)
}

pub fn pure(value: a) -> Lexer(a, r) {
  use state, succ, _ <- Lexer
  succ(state, Span(start: state.pos, end: state.pos), value)
}

pub fn die() -> Lexer(a, r) {
  use _, _, fail <- Lexer
  fail()
}

pub fn perform(l: Lexer(a, r), f: fn(a) -> Lexer(b, r)) -> Lexer(b, r) {
  use state, succ, fail <- Lexer

  l.run(
    state,
    fn(state, span1, x) {
      f(x).run(
        state,
        fn(state, span2, y) {
          let span = Span(start: span1.start, end: span2.end)
          succ(state, span, y)
        },
        fail
      )
    },
    fail
  )
}

pub fn map(l: Lexer(a, r), f: fn(a) -> b) -> Lexer(b, r) {
  perform(l, fn(x) { pure(f(x)) })
}

pub fn replace(l: Lexer(a, r), x: b) -> Lexer(b, r) {
  perform(l, fn(_) { pure(x) })
}

pub fn extract_span(l: Lexer(a, r)) -> Lexer(#(a, Span), r) {
  use state, succ, fail <- Lexer
  l.run(state, fn(state, span, x) { succ(state, span, #(x, span)) }, fail)
}

pub fn eat(pred: fn(String) -> Bool) -> Lexer(String, r) {
  use LexerState(input:, pos:), succ, fail <- Lexer

  case string.pop_grapheme(input) {
    Error(_) -> fail()

    Ok(#(char, input)) -> {
      case pred(char) {
        False -> fail()

        True -> {
          let start_pos = pos
          let pos = case char {
            "\n" -> advance_lines(start_pos, 1)
            _ -> advance_chars(start_pos, 1)
          }

          succ(LexerState(input:, pos:), Span(start: start_pos, end: pos), char)
        }
      }
    }
  }
}

pub fn oneof(lexers: List(Lexer(a, r))) -> Lexer(a, r) {
  use state, succ, fail <- Lexer

  let f =
    list.fold_right(lexers, fn() { fail() }, fn(next, l) {
      fn() { l.run(state, succ, next) }
    })

  f()
}

pub fn eat_exact(str: String) -> Lexer(Nil, r) {
  let assert [lexer, ..lexers] =
    string.to_graphemes(str)
    |> list.map(fn(c) { fn(x) { x == c } })
    |> list.map(eat)

  list.fold(lexers, lexer, fn(l1, l2) { perform(l1, fn(_) { l2 }) })
  |> map(fn(_) { Nil })
}

pub fn many(l: Lexer(a, #(LexerState, List(a)))) -> Lexer(List(a), r) {
  use state, succ, _ <- Lexer
  let start_pos = state.pos

  let #(state, xs) = many_loop(l, state, [])
  let end_pos = state.pos

  succ(state, Span(start: start_pos, end: end_pos), list.reverse(xs))
}

fn many_loop(
  l: Lexer(a, #(LexerState, List(a))),
  state: LexerState,
  result: List(a)
) -> #(LexerState, List(a)) {
  l.run(
    state,
    fn(state, _, x) { many_loop(l, state, [x, ..result]) },
    fn() { #(state, result) }
  )
}

pub fn some(l: Lexer(a, #(LexerState, List(a)))) -> Lexer(List(a), r) {
  use xs <- perform(many(l))
  case xs {
    [] -> die()
    xs -> pure(xs)
  }
}

pub fn lex(l: Lexer(a, Option(#(LexerState, a))), input: String) -> Option(#(LexerState, a)) {
  let state = LexerState(input:, pos: Pos(line: 1, char: 1))
  l.run(state, fn(state, _, x) { Some(#(state, x)) }, fn() { None })
}
