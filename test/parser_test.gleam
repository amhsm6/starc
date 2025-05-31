import gleam/result
import gleeunit/should

import starc/lexer
import starc/lexer/token.{type Token}
import starc/parser/core.{type Parser} as parser

fn parse_eq() -> Parser(Token, r) {
  use t <- parser.perform(parser.eat_exact(token.TokenEquals))
  use _ <- parser.perform(
    parser.eat_exact(token.TokenLBrace) |> parser.with_message("Expected {"),
  )
  use _ <- parser.perform(
    parser.eat_exact(token.TokenRBrace) |> parser.with_message("Expected }"),
  )
  parser.pure(t)
}

fn parse_neq() -> Parser(Token, r) {
  use t <- parser.perform(parser.eat_exact(token.TokenNotEquals))
  use _ <- parser.perform(
    parser.eat_exact(token.TokenFn) |> parser.with_message("Expected fn"),
  )
  use _ <- parser.perform(
    parser.eat_exact(token.TokenStar) |> parser.with_message("Expected *"),
  )
  parser.pure(t)
}

fn parse_if() -> Parser(Nil, r) {
  use _ <- parser.perform(
    parser.eat_exact(token.TokenIf) |> parser.with_message("Expected token IF"),
  )
  use _ <- parser.perform(
    parser.eat_exact(token.TokenLParen)
    |> parser.with_message("Expected token LParen"),
  )

  use token <- parser.perform(parser.oneof(
    [parse_eq(), parse_neq()],
    "Expected == or !=",
  ))

  parser.pure(Nil)
}

pub fn parse_numbers() -> Parser(List(Int), r) {
  let p =
    parser.eat(fn(t) {
      case t {
        token.TokenInt(_) -> True
        _ -> False
      }
    })
    |> parser.perform(fn(t) {
      let assert token.TokenInt(n) = t
      parser.pure(n)
    })

  parser.many_sep(p, parser.eat_exact(token.TokenPlus))
}

pub fn parse_equals() -> Parser(List(Token), r) {
  parser.many_sep(
    parser.oneof([parse_eq(), parse_neq()], "expected == or !="),
    parser.eat_exact(token.TokenPlus),
  )
}

type GeneralError {
  LexerError(lexer.LexerError)
  ParserError(String)
}

pub fn parse_if_test() {
  let res = {
    use tokens <- result.try(
      lexer.lex("if (!=fn *    })") |> result.map_error(LexerError),
    )

    parse_if().run(
      parser.begin(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(#(
    parser.Consumed([token.TokenRBrace, token.TokenRParen, token.TokenEOF]),
    Nil,
  ))

  let res = {
    use tokens <- result.try(
      lexer.lex("if   ( =={") |> result.map_error(LexerError),
    )

    parse_if().run(
      parser.begin(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_error(res)
  |> should.equal(ParserError("Expected }"))

  let res = {
    use tokens <- result.try(
      lexer.lex("if (!=fn *    })") |> result.map_error(LexerError),
    )

    parse_if().run(
      parser.begin(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(#(
    parser.Consumed([token.TokenRBrace, token.TokenRParen, token.TokenEOF]),
    Nil,
  ))
}

pub fn many_test() {
  let res = {
    use tokens <- result.try(
      lexer.lex("12321 + 43+ 324 +4+5345+1+++") |> result.map_error(LexerError),
    )

    parse_numbers().run(
      parser.begin(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(
    #(parser.Consumed([token.TokenPlus, token.TokenPlus, token.TokenEOF]), [
      12_321, 43, 324, 4, 5345, 1,
    ]),
  )

  let res = {
    use tokens <- result.try(
      lexer.lex("=={}+!=fn*+=={}+=={}+!=fn*") |> result.map_error(LexerError),
    )

    parse_equals().run(
      parser.begin(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(
    #(parser.Consumed([token.TokenEOF]), [
      token.TokenEquals,
      token.TokenNotEquals,
      token.TokenEquals,
      token.TokenEquals,
      token.TokenNotEquals,
    ]),
  )
  let res = {
    use tokens <- result.try(
      lexer.lex("=={}+!=fn*+=={}+==}+!=fn*") |> result.map_error(LexerError),
    )

    parse_equals().run(
      parser.begin(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_error(res)
  |> should.equal(ParserError("Expected {"))
}
