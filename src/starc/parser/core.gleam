import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

import starc/lexer/token.{type Token}

pub type Parser(a, r) {
  Parser(
    run: fn(
      ParserState,
      fn(a, Message) -> r,
      fn(ParserState, a, Message) -> r,
      fn(Message) -> r,
      fn(Message) -> r,
    ) ->
      r,
  )
}

pub type ParserState {
  ParserState(tokens: List(Token), pos: Pos)
}

pub type Message {
  Message(pos: Pos, unexpected: String, expected: List(String))
}

pub type Pos =
  Int

pub opaque type ParserResult(a) {
  OkEmpty(a, Message)
  OkConsumed(ParserState, a, Message)
  FailEmpty(Message)
  FailConsumed(Message)
}

fn advance_token(pos: Pos) -> Pos {
  pos + 1
}

fn merge(msg1: Message, msg2: Message) -> Message {
  Message(msg1.pos, msg1.unexpected, list.append(msg1.expected, msg2.expected))
}

pub fn pure(x: a) -> Parser(a, r) {
  use ParserState(_, pos), ok_empty, _, _, _ <- Parser
  ok_empty(x, Message(pos, "", []))
}

pub fn perform(p: Parser(a, r), f: fn(a) -> Parser(b, r)) -> Parser(b, r) {
  use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

  p.run(
    state,
    fn(x, msg) {
      f(x).run(
        state,
        fn(y, msg2) { ok_empty(y, merge(msg, msg2)) },
        ok_consumed,
        fn(msg2) { fail_empty(merge(msg, msg2)) },
        fail_consumed,
      )
    },
    fn(state, x, _) {
      f(x).run(
        state,
        fn(y, msg) { ok_consumed(state, y, msg) },
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

pub fn eat(pred: fn(Token) -> Bool) -> Parser(Token, r) {
  use ParserState(tokens, pos), _, ok_consumed, fail_empty, _ <- Parser
  case tokens {
    [t, ..ts] ->
      case pred(t) {
        True -> {
          let pos = advance_token(pos)
          ok_consumed(ParserState(ts, pos), t, Message(pos, "", []))
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
    fn(x, msg) {
      p2.run(
        state,
        fn(_, msg2) { ok_empty(x, merge(msg, msg2)) },
        ok_consumed,
        fn(msg2) { ok_empty(x, merge(msg, msg2)) },
        fail_consumed,
      )
    },
    ok_consumed,
    fn(msg) {
      p2.run(
        state,
        fn(x, msg2) { ok_empty(x, merge(msg, msg2)) },
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
    fn(x, msg2) { ok_empty(x, Message(msg2.pos, msg2.unexpected, [msg])) },
    ok_consumed,
    fn(msg2) { fail_empty(Message(msg2.pos, msg2.unexpected, [msg])) },
    fail_consumed,
  )
}

// pub fn many(p: Parser(a, ParserResult(List(a)))) -> Parser(List(a), r) {
//   use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser
//   case many_loop(p, state, [], False) {
//     OkEmpty(x) -> ok_empty(list.reverse(x))
//     OkConsumed(state, x) -> ok_consumed(state, list.reverse(x))
//     FailEmpty(msg) -> fail_empty(msg)
//     FailConsumed(state, msg) -> fail_consumed(state, msg)
//   }
// }

// fn many_loop(
//   p: Parser(a, ParserResult(List(a))),
//   state: ParserState,
//   result: List(a),
//   consumed: Bool,
// ) -> ParserResult(List(a)) {
//   p.run(
//     state,
//     fn(x, msg) { many_loop(p, state, [x, ..result], consumed) },
//     fn(state, x, msg) { many_loop(p, state, [x, ..result], True) },
//     fn(_) {
//       case consumed {
//         True -> OkConsumed(state, result)
//         False -> OkEmpty(result)
//       }
//     },
//     FailConsumed,
//   )
// }

pub fn maybe(p: Parser(a, r)) -> Parser(Option(a), r) {
  choose(map(p, Some), pure(None))
}

pub fn oneof(parsers: List(Parser(a, r))) -> Parser(a, r) {
  let assert [p, ..ps] = parsers
  list.fold(ps, p, fn(p1, p2) { choose(p1, p2) })
}

// fn generalize(p: Parser(a, ParserResult(a))) -> Parser(a, r) {
//   use state, ok_empty, ok_consumed, fail_empty, fail_consumed <- Parser

//   let res = p.run(state, OkEmpty, OkConsumed, FailEmpty, FailConsumed)
//   case res {
//     OkEmpty(x) -> ok_empty(x)
//     OkConsumed(state, x) -> ok_consumed(state, x)
//     FailEmpty(msg) -> fail_empty(msg)
//     FailConsumed(state, msg) -> fail_consumed(state, msg)
//   }
// }

// pub fn sep_by(
//   p: Parser(a, ParserResult(List(a))),
//   sep: Parser(b, ParserResult(List(a))),
// ) -> Parser(List(a), r) {
//   generalize(choose(
//     {
//       use x <- perform(p)
//       use xs <- perform(
//         many({
//           use _ <- perform(sep)
//           use y <- perform(p)
//           pure(y)
//         }),
//       )
//       pure([x, ..xs])
//     },
//     pure([]),
//   ))
// }

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
  p: Parser(a, Result(a, Message)),
  tokens: List(Token),
) -> Result(a, Message) {
  let state = ParserState(tokens, 1)
  p.run(state, fn(x, _) { Ok(x) }, fn(_, x, _) { Ok(x) }, Error, Error)
}
