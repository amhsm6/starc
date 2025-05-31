import gleam/list
import gleam/option.{type Option, None, Some}

import starc/lexer/token.{type Token}

pub type Parser(a, r) {
  Parser(
    run: fn(ParserState, fn(ParserState, a) -> r, fn(ParserState, String) -> r) ->
      r,
  )
}

pub type ParserState {
  Unconsumed(input: List(Token))
  Consumed(input: List(Token))
}

fn unconsume(state: ParserState) -> ParserState {
  case state {
    Unconsumed(_) -> state
    Consumed(input) -> Unconsumed(input)
  }
}

pub fn eat(pred: fn(Token) -> Bool) -> Parser(Token, r) {
  use state, succ, fail <- Parser

  case state {
    Unconsumed([t, ..rest]) -> {
      case pred(t) {
        True -> succ(Consumed(rest), t)
        False -> fail(state, "no match")
      }
    }
    Consumed([t, ..rest]) -> {
      case pred(t) {
        True -> succ(Consumed(rest), t)
        False -> fail(state, "no match")
      }
    }
    Unconsumed([]) -> fail(state, "eof")
    Consumed([]) -> fail(state, "eof")
  }
}

pub fn eat_exact(t: Token) -> Parser(Token, r) {
  eat(fn(x) { x == t })
}

pub fn pure(value: a) -> Parser(a, r) {
  Parser(fn(state, succ, _) { succ(state, value) })
}

pub fn die(message: String) -> Parser(a, r) {
  Parser(fn(state, _, fail) { fail(state, message) })
}

pub fn perform(p: Parser(a, r), f: fn(a) -> Parser(b, r)) -> Parser(b, r) {
  use state, succ, fail <- Parser
  p.run(state, fn(state, x) { f(x).run(state, succ, fail) }, fail)
}

pub fn with_message(p: Parser(a, r), msg: String) -> Parser(a, r) {
  use state, succ, fail <- Parser
  p.run(state, succ, fn(state, _) { fail(state, msg) })
}

pub fn oneof(parsers: List(Parser(a, r)), nomatch_msg: String) -> Parser(a, r) {
  use state, succ, fail <- Parser

  let f =
    list.fold_right(parsers, fn() { fail(state, nomatch_msg) }, fn(next, p) {
      fn() {
        p.run(unconsume(state), succ, fn(state, msg) {
          case state {
            Consumed(_) -> fail(state, msg)
            Unconsumed(_) -> next()
          }
        })
      }
    })

  f()
}

pub fn maybe(p: Parser(a, r)) -> Parser(Option(a), r) {
  use state, succ, fail <- Parser

  p.run(
    unconsume(state),
    fn(state, x) { succ(state, Some(x)) },
    fn(fail_state, msg) {
      case fail_state {
        Consumed(_) -> fail(fail_state, msg)
        Unconsumed(_) -> succ(state, None)
      }
    },
  )
}

pub fn many(
  p: Parser(a, Result(#(ParserState, List(a)), #(ParserState, String))),
) -> Parser(List(a), r) {
  use state, succ, fail <- Parser

  case many_loop(p, state, []) {
    Ok(#(state, xs)) -> succ(state, xs)
    Error(#(state, msg)) -> fail(state, msg)
  }
}

fn many_loop(
  p: Parser(a, Result(#(ParserState, List(a)), #(ParserState, String))),
  state: ParserState,
  result: List(a),
) -> Result(#(ParserState, List(a)), #(ParserState, String)) {
  maybe(p).run(
    state,
    fn(state, x) {
      case x {
        None -> Ok(#(state, result))
        Some(x) -> many_loop(p, state, [x, ..result])
      }
    },
    fn(state, msg) { Error(#(state, msg)) },
  )
}

pub fn many_sep(p: Parser(a, r), sep: Parser(b, r)) -> Parser(List(a), r) {
  use state, succ, fail <- Parser

  maybe(p).run(
    state,
    fn(state, parsed_x) {
      case parsed_x {
        None -> succ(state, [])
        Some(x) -> {
          maybe(sep).run(
            state,
            fn(state, parsed_sep) {
              case parsed_sep {
                None -> succ(state, [x])
                Some(_) -> {
                  many_sep(p, sep).run(
                    state,
                    fn(state, xs) { succ(state, [x, ..xs]) },
                    fail,
                  )
                }
              }
            },
            fail,
          )
        }
      }
    },
    fail,
  )
}
