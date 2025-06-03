import gleam/list
import gleam/option.{type Option, None, Some}

import starc/lexer/token.{type Token}

pub type Parser(a, r) {
  Parser(
    run: fn(
      ParserState,
      fn(a) -> r,
      fn(ParserState, a) -> r,
      fn(String) -> r,
      fn(ParserState, String) -> r,
    ) ->
      r,
  )
}

type ParserState =
  List(Token)

pub opaque type ParserResult(a) {
  OkEmpty(a)
  OkConsumed(ParserState, a)
  FailEmpty(String)
  FailConsumed(ParserState, String)
}

pub fn pure(x: a) -> Parser(a, r) {
  use _, ok_empty, _, _, _ <- Parser
  ok_empty(x)
}

pub fn perform(p: Parser(a, r), f: fn(a) -> Parser(b, r)) -> Parser(b, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p.run(
    state,
    fn(x) { f(x).run(state, ok_empty, ok_consumed, fail_empty, fail_consumed) },
    fn(state, x) {
      f(x).run(
        state,
        ok_consumed(state, _),
        ok_consumed,
        fail_consumed(state, _),
        fail_consumed,
      )
    },
    fail_empty,
    fail_consumed,
  )
}

pub fn map(p: Parser(a, r), f: fn(a) -> b) -> Parser(b, r) {
  perform(p, fn(x) { pure(f(x)) })
}

pub fn eat(pred: fn(Token) -> Bool) -> Parser(Token, r) {
  use state, _, ok_consumed, fail_empty, _ <- Parser
  case state {
    [t, ..ts] ->
      case pred(t) {
        True -> ok_consumed(ts, t)
        False -> fail_empty("no match")
      }
    _ -> fail_empty("eof")
  }
}

pub fn choose(p1: Parser(a, r), p2: Parser(a, r)) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p1.run(
    state,
    fn(x) {
      p2.run(
        state,
        fn(_) { ok_empty(x) },
        ok_consumed,
        fn(_) { ok_empty(x) },
        fail_consumed,
      )
    },
    ok_consumed,
    fn(_) { p2.run(state, ok_empty, ok_consumed, fail_empty, fail_consumed) },
    fail_consumed,
  )
}

pub fn try(p: Parser(a, r)) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, _ <- Parser
  p.run(state, ok_empty, ok_consumed, fail_empty, fn(_, msg) { fail_empty(msg) })
}

pub fn many(p: Parser(a, ParserResult(List(a)))) -> Parser(List(a), r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser
  case many_loop(p, state, [], False) {
    OkEmpty(x) -> ok_empty(list.reverse(x))
    OkConsumed(state, x) -> ok_consumed(state, list.reverse(x))
    FailEmpty(msg) -> fail_empty(msg)
    FailConsumed(state, msg) -> fail_consumed(state, msg)
  }
}

fn many_loop(
  p: Parser(a, ParserResult(List(a))),
  state: ParserState,
  result: List(a),
  consumed: Bool,
) -> ParserResult(List(a)) {
  p.run(
    state,
    fn(x) { many_loop(p, state, [x, ..result], consumed) },
    fn(state, x) { many_loop(p, state, [x, ..result], True) },
    fn(_) {
      case consumed {
        True -> OkConsumed(state, result)
        False -> OkEmpty(result)
      }
    },
    fn(state, msg) { FailConsumed(state, msg) },
  )
}

pub fn maybe(p: Parser(a, r)) -> Parser(Option(a), r) {
  choose(map(p, Some), pure(None))
}

pub fn oneof(parsers: List(Parser(a, r))) -> Parser(a, r) {
  let assert [p, ..ps] = parsers
  list.fold(ps, p, fn(p1, p2) { choose(p1, p2) })
}

fn generalize(p: Parser(a, ParserResult(a))) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  let res = p.run(state, OkEmpty, OkConsumed, FailEmpty, FailConsumed)
  case res {
    OkEmpty(x) -> ok_empty(x)
    OkConsumed(state, x) -> ok_consumed(state, x)
    FailEmpty(msg) -> fail_empty(msg)
    FailConsumed(state, msg) -> fail_consumed(state, msg)
  }
}

pub fn sep_by(
  p: Parser(a, ParserResult(List(a))),
  sep: Parser(b, ParserResult(List(a))),
) -> Parser(List(a), r) {
  generalize(choose(
    {
      use x <- perform(p)
      use xs <- perform(
        many({
          use _ <- perform(sep)
          use y <- perform(p)
          pure(y)
        }),
      )
      pure([x, ..xs])
    },
    pure([]),
  ))
}

pub fn eat_exact(t: Token) -> Parser(Token, r) {
  eat(fn(x) { x == t })
}

pub fn parse(
  p: Parser(a, Result(#(List(Token), a), #(List(Token), String))),
  tokens: List(Token),
) -> Result(#(List(Token), a), #(List(Token), String)) {
  p.run(
    tokens,
    fn(x) { Ok(#(tokens, x)) },
    fn(state, x) { Ok(#(state, x)) },
    fn(msg) { Error(#(tokens, msg)) },
    fn(state, msg) { Error(#(state, msg)) },
  )
}
