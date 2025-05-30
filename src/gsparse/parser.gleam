import gleam/list
import gleam/option.{type Option, None, Some}

import gsparse/lexer.{type Token}

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

pub type ParserError {
  ParserError(before: Token, reason: String)
}

fn unconsume(state: ParserState) -> ParserState {
  case state {
    Unconsumed(_) -> state
    Consumed(input) -> Unconsumed(input)
  }
}

fn eat(pred: fn(Token) -> Bool) -> Parser(Token, r) {
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

fn eat_exact(t: Token) -> Parser(Token, r) {
  eat(fn(x) { x == t })
}

fn pure(value: a) -> Parser(a, r) {
  Parser(fn(state, succ, _) { succ(state, value) })
}

fn die(message: String) -> Parser(a, r) {
  Parser(fn(state, _, fail) { fail(state, message) })
}

fn after(p: Parser(a, r), f: fn(a) -> Parser(b, r)) -> Parser(b, r) {
  use state, succ, fail <- Parser
  p.run(state, fn(state, x) { f(x).run(state, succ, fail) }, fail)
}

fn with_message(p: Parser(a, r), msg: String) -> Parser(a, r) {
  use state, succ, fail <- Parser
  p.run(state, succ, fn(state, _) { fail(state, msg) })
}

fn oneof(parsers: List(Parser(a, r)), nomatch_msg: String) -> Parser(a, r) {
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

fn maybe(p: Parser(a, r)) -> Parser(Option(a), r) {
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

fn many(p: Parser(a, r)) -> Parser(List(a), r) {
  use state, succ, fail <- Parser

  p.run(
    state,
    fn(state, x) {
      many(p).run(state, fn(state, xs) { succ(state, [x, ..xs]) }, fail)
    },
    fn(state, _) { succ(state, []) },
  )
}

fn many_sep(p: Parser(a, r), sep: Parser(b, r)) -> Parser(List(a), r) {
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

fn parse_eq() -> Parser(Token, r) {
  use t <- after(eat_exact(lexer.TokenEquals))
  use _ <- after(eat_exact(lexer.TokenLBrace) |> with_message("Expected {"))
  use _ <- after(eat_exact(lexer.TokenRBrace) |> with_message("Expected }"))
  pure(t)
}

fn parse_neq() -> Parser(Token, r) {
  use t <- after(eat_exact(lexer.TokenNotEquals))
  use _ <- after(eat_exact(lexer.TokenFn) |> with_message("Expected fn"))
  use _ <- after(eat_exact(lexer.TokenStar) |> with_message("Expected *"))
  pure(t)
}

pub fn parse_if() -> Parser(Nil, r) {
  use _ <- after(eat_exact(lexer.TokenIf) |> with_message("Expected token IF"))
  use _ <- after(
    eat_exact(lexer.TokenLParen) |> with_message("Expected token LParen"),
  )

  use token <- after(oneof([parse_eq(), parse_neq()], "Expected == or !="))

  pure(Nil)
}

pub fn parse_numbers() -> Parser(List(Int), r) {
  let p =
    eat(fn(t) {
      case t {
        lexer.TokenInt(_) -> True
        _ -> False
      }
    })
    |> after(fn(t) {
      let assert lexer.TokenInt(n) = t
      pure(n)
    })

  many_sep(p, eat_exact(lexer.TokenPlus))
}

pub fn parse_equals() -> Parser(List(Token), r) {
  many_sep(
    oneof([parse_eq(), parse_neq()], "expected == or !="),
    eat_exact(lexer.TokenPlus),
  )
}
