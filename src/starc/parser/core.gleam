import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/string

import starc/lexer/token.{type Token}

pub type Parser(a, r) {
  Parser(
    run: fn(
      ParserState,
      fn(ParserState, a, Message) -> r,
      fn(ParserState, a, Message) -> r,
      fn(Message) -> r,
      fn(Message) -> r,
    ) ->
      r,
  )
}

pub type ParserState {
  ParserState(tokens: List(Token), pos: Pos, ignore_newline: Bool)
}

pub type Message {
  Message(pos: Pos, unexpected: String, expected: List(String))
}

pub type Pos =
  Int

fn advance_token(pos: Pos) -> Pos {
  pos + 1
}

fn merge(msg1: Message, msg2: Message) -> Message {
  Message(msg1.pos, msg1.unexpected, list.append(msg1.expected, msg2.expected))
}

pub fn pure(x: a) -> Parser(a, r) {
  use ParserState(_, pos, _) as state, ok_empty, _, _, _ <- Parser
  ok_empty(state, x, Message(pos, "", []))
}

pub fn die() -> Parser(a, r) {
  use ParserState(_, pos, _), _, _, fail_empty, _ <- Parser
  fail_empty(Message(pos, "", []))
}

pub fn perform(p: Parser(a, r), f: fn(a) -> Parser(b, r)) -> Parser(b, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p.run(
    state,
    fn(state, x, msg) {
      f(x).run(
        state,
        fn(state, y, msg2) { ok_empty(state, y, merge(msg, msg2)) },
        ok_consumed,
        fn(msg2) { fail_empty(merge(msg, msg2)) },
        fail_consumed,
      )
    },
    fn(state, x, _) {
      f(x).run(
        state,
        fn(state, y, msg) { ok_consumed(state, y, msg) },
        ok_consumed,
        fail_consumed,
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

pub fn ignore_newline(p: Parser(a, r)) -> Parser(a, r) {
  use
    ParserState(tokens, pos, _),
    ok_empty,
    ok_consumed,
    fail_empty,
    fail_consumed
  <- Parser

  p.run(
    ParserState(tokens, pos, True),
    fn(state, x, msg) {
      ok_empty(ParserState(state.tokens, state.pos, False), x, msg)
    },
    fn(state, x, msg) {
      ok_consumed(ParserState(state.tokens, state.pos, False), x, msg)
    },
    fail_empty,
    fail_consumed,
  )
}

pub fn block_newline(p: Parser(a, r)) -> Parser(a, r) {
  use
    ParserState(tokens, pos, _),
    ok_empty,
    ok_consumed,
    fail_empty,
    fail_consumed
  <- Parser

  p.run(
    ParserState(tokens, pos, False),
    fn(state, x, msg) {
      ok_empty(ParserState(state.tokens, state.pos, True), x, msg)
    },
    fn(state, x, msg) {
      ok_consumed(ParserState(state.tokens, state.pos, True), x, msg)
    },
    fail_empty,
    fail_consumed,
  )
}

pub fn eat(pred: fn(Token) -> Bool) -> Parser(Token, r) {
  use ParserState(tokens, pos, ignore_newline), _, ok_consumed, fail_empty, _ <-
    Parser

  let is_newline = fn(t) {
    case t {
      token.TokenNewline -> True
      _ -> False
    }
  }

  let tokens = case ignore_newline {
    True -> list.split_while(tokens, is_newline) |> pair.second()
    False -> tokens
  }

  case tokens {
    [t, ..ts] ->
      case pred(t) {
        True -> {
          let pos = advance_token(pos)
          ok_consumed(
            ParserState(ts, pos, ignore_newline),
            t,
            Message(pos, "", []),
          )
        }

        False -> fail_empty(Message(pos, string.inspect(t), []))
      }

    _ -> fail_empty(Message(pos, "eof", []))
  }
}

pub fn choose(p1: Parser(a, r), p2: Parser(a, r)) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p1.run(
    state,
    fn(state, x, msg) {
      p2.run(
        state,
        fn(state, _, msg2) { ok_empty(state, x, merge(msg, msg2)) },
        ok_consumed,
        fn(msg2) { ok_empty(state, x, merge(msg, msg2)) },
        fail_consumed,
      )
    },
    ok_consumed,
    fn(msg) {
      p2.run(
        state,
        fn(state, x, msg2) { ok_empty(state, x, merge(msg, msg2)) },
        ok_consumed,
        fn(msg2) { fail_empty(merge(msg, msg2)) },
        fail_consumed,
      )
    },
    fail_consumed,
  )
}

pub fn try(p: Parser(a, r)) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, _ <- Parser
  p.run(state, ok_empty, ok_consumed, fail_empty, fail_empty)
}

pub fn many(p: Parser(a, r)) -> Parser(List(a), r) {
  use x <- perform(maybe(p))
  case x {
    None -> pure([])
    Some(x) -> {
      use xs <- perform(many(p))
      pure([x, ..xs])
    }
  }
}

pub fn expect(p: Parser(a, r), msg: String) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p.run(
    state,
    fn(state, x, original_msg) {
      ok_empty(
        state,
        x,
        Message(original_msg.pos, original_msg.unexpected, [msg]),
      )
    },
    ok_consumed,
    fn(original_msg) {
      fail_empty(Message(original_msg.pos, original_msg.unexpected, [msg]))
    },
    fail_consumed,
  )
}

pub fn maybe(p: Parser(a, r)) -> Parser(Option(a), r) {
  choose(map(p, Some), pure(None))
}

pub fn oneof(parsers: List(Parser(a, r))) -> Parser(a, r) {
  let assert [p, ..ps] = parsers
  list.fold(ps, p, fn(p1, p2) { choose(p1, p2) })
}

pub fn sep_by(p: Parser(a, r), sep: Parser(b, r)) {
  choose(
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
  )
}

pub fn eat_exact(t: Token) -> Parser(Token, r) {
  eat(fn(x) { x == t })
  |> expect(string.inspect(t))
}

pub fn parse(
  p: Parser(a, Result(#(a, List(Token)), Message)),
  tokens: List(Token),
) -> Result(#(a, List(Token)), Message) {
  let state = ParserState(tokens, 1, True)
  p.run(
    state,
    fn(state, x, _) { Ok(#(x, state.tokens)) },
    fn(state, x, _) { Ok(#(x, state.tokens)) },
    Error,
    Error,
  )
}
