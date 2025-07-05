import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/string

import starc/lexer/token.{type Pos, type Token, type TokenType, Pos, Token}

pub type Parser(a, r) {
  Parser(run: fn(
    ParserState,
    fn(ParserState, a, Message) -> r,
    fn(ParserState, a, Message) -> r,
    fn(Message) -> r,
    fn(Message) -> r,
  ) -> r)
}

pub type ParserState {
  ParserState(tokens: List(Token), pos: Pos, ignore_newline: Bool)
}

pub type Message {
  Message(pos: Pos, unexpected: String, expected: List(String))
}

fn merge(msg1: Message, msg2: Message) -> Message {
  Message(..msg1, expected: list.append(msg1.expected, msg2.expected))
}

pub fn pure(x: a) -> Parser(a, r) {
  use state, ok_empty, _, _, _ <- Parser
  ok_empty(state, x, Message(pos: state.pos, unexpected: "", expected: []))
}

pub fn die() -> Parser(a, r) {
  use ParserState(pos:, ..), _, _, fail_empty, _ <- Parser
  fail_empty(Message(pos:, unexpected: "", expected: []))
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
        fail_consumed
      )
    },
    fn(state, x, _) {
      f(x).run(state, ok_consumed, ok_consumed, fail_consumed, fail_consumed)
    },
    fail_empty,
    fail_consumed
  )
}

pub fn map(p: Parser(a, r), f: fn(a) -> b) -> Parser(b, r) {
  perform(p, fn(x) { pure(f(x)) })
}

pub fn block_newline(p: Parser(a, r)) -> Parser(a, r) {
  use
    ParserState(ignore_newline:, ..) as state,
    ok_empty,
    ok_consumed,
    fail_empty,
    fail_consumed
  <- Parser

  p.run(
    ParserState(..state, ignore_newline: False),
    fn(state, x, msg) {
      ok_empty(ParserState(..state, ignore_newline:), x, msg)
    },
    fn(state, x, msg) {
      ok_consumed(ParserState(..state, ignore_newline:), x, msg)
    },
    fail_empty,
    fail_consumed
  )
}

pub fn ignore_newline(p: Parser(a, r)) -> Parser(a, r) {
  use
    ParserState(ignore_newline:, ..) as state,
    ok_empty,
    ok_consumed,
    fail_empty,
    fail_consumed
  <- Parser

  p.run(
    ParserState(..state, ignore_newline: True),
    fn(state, x, msg) {
      ok_empty(ParserState(..state, ignore_newline:), x, msg)
    },
    fn(state, x, msg) {
      ok_consumed(ParserState(..state, ignore_newline:), x, msg)
    },
    fail_empty,
    fail_consumed
  )
}

pub fn eat_newlines(p: Parser(a, r)) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  let is_newline = fn(t: Token) {
    case t.token_type {
      token.TokenNewline -> True
      _ -> False
    }
  }

  p.run(
    ParserState(
      ..state,
      tokens: list.split_while(state.tokens, is_newline) |> pair.second(),
    ),
    ok_empty,
    ok_consumed,
    fail_empty,
    fail_consumed
  )
}

pub fn eat(pred: fn(TokenType) -> Bool) -> Parser(Token, r) {
  use ParserState(tokens:, pos:, ignore_newline:), _, ok_consumed, fail_empty, _ <- Parser

  let is_newline = fn(t: Token) {
    case t.token_type {
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
      case pred(t.token_type) {
        True ->
          ok_consumed(
            ParserState(tokens: ts, pos: t.span.end, ignore_newline:),
            t,
            Message(pos: t.span.end, unexpected: "", expected: [])
          )

        False ->
          fail_empty(
            Message(
              pos: t.span.start,
              unexpected: string.inspect(t.token_type),
              expected: []
            )
          )
      }

    _ -> fail_empty(Message(pos:, unexpected: "eof", expected: []))
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
        fail_consumed
      )
    },
    ok_consumed,
    fn(msg) {
      p2.run(
        state,
        fn(state, x, msg2) { ok_empty(state, x, merge(msg, msg2)) },
        ok_consumed,
        fn(msg2) { fail_empty(merge(msg, msg2)) },
        fail_consumed
      )
    },
    fail_consumed
  )
}

pub fn try(p: Parser(a, r)) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, _ <- Parser
  p.run(state, ok_empty, ok_consumed, fail_empty, fail_empty)
}

pub fn expect(p: Parser(a, r), msg: String) -> Parser(a, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p.run(
    state,
    fn(state, x, original_msg) {
      ok_empty(state, x, Message(..original_msg, expected: [msg]))
    },
    ok_consumed,
    fn(original_msg) { fail_empty(Message(..original_msg, expected: [msg])) },
    fail_consumed
  )
}

pub fn many(p: Parser(a, r)) -> Parser(List(a), r) {
  choose(
    {
      use x <- perform(p)
      use xs <- perform(many(p))
      pure([x, ..xs])
    },
    pure([])
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
    pure([])
  )
}

pub fn eat_exact(t: TokenType) -> Parser(Token, r) {
  eat(fn(x) { x == t })
  |> expect(string.inspect(t))
}

pub fn parse(
  p: Parser(a, Result(#(a, List(Token)), Message)),
  tokens: List(Token),
) -> Result(#(a, List(Token)), Message) {
  let start_pos = case tokens {
    [] -> Pos(line: 1, char: 1)
    [Token(span:, ..), ..] -> span.start
  }
  let state = ParserState(tokens:, pos: start_pos, ignore_newline: True)

  p.run(
    state,
    fn(state, x, _) { Ok(#(x, state.tokens)) },
    fn(state, x, _) { Ok(#(x, state.tokens)) },
    Error,
    Error
  )
}
