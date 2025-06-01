import gleam/result
import gleeunit/should

import starc/lexer
import starc/lexer/token.{type Token}
import starc/parser/core.{type Parser, type ParserState} as parser

fn parse_eq() -> Parser(Token, r) {
  use t <- parser.perform(parser.eat_exact(token.TokenEquals))
  use _ <- parser.perform(parser.eat_exact(token.TokenLBrace))
  use _ <- parser.perform(parser.eat_exact(token.TokenRBrace))
  parser.pure(t)
}

fn parse_neq() -> Parser(Token, r) {
  use t <- parser.perform(parser.eat_exact(token.TokenNotEquals))
  use _ <- parser.perform(parser.eat_exact(token.TokenFn))
  use _ <- parser.perform(parser.eat_exact(token.TokenStar))
  parser.pure(t)
}

fn parse_if() -> Parser(Nil, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenIf))
  use _ <- parser.perform(parser.eat_exact(token.TokenLParen))

  use _ <- parser.perform(parser.oneof(
    [parse_eq(), parse_neq()],
    "Expected == or !=",
  ))

  parser.pure(Nil)
}

pub fn parse_numbers() -> Parser(List(Int), r) {
  let p =
    parser.eat(
      fn(t) {
        case t {
          token.TokenInt(_) -> True
          _ -> False
        }
      },
      "Expected int",
    )
    |> parser.perform(fn(t) {
      let assert token.TokenInt(n) = t
      parser.pure(n)
    })

  parser.sep_by(p, parser.eat_exact(token.TokenPlus))
}

pub fn parse_equals() -> Parser(List(Token), r) {
  parser.sep_by(
    parser.oneof([parse_eq(), parse_neq()], "expected == or !="),
    parser.eat_exact(token.TokenPlus),
  )
}

type GeneralError {
  LexerError(lexer.LexerError)
  ParserError(#(ParserState, String))
}

pub fn parse_if_test() {
  let res = {
    use tokens <- result.try(
      lexer.lex_program("if (!=fn *    })") |> result.map_error(LexerError),
    )

    parser.parse(parse_if(), tokens)
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(#(
    parser.Consumed([token.TokenRBrace, token.TokenRParen, token.TokenEOF]),
    Nil,
  ))

  let res = {
    use tokens <- result.try(
      lexer.lex_program("if   ( =={") |> result.map_error(LexerError),
    )

    parser.parse(parse_if(), tokens)
    |> result.map_error(ParserError)
  }

  should.be_error(res)
  |> should.equal(
    ParserError(#(parser.Consumed([token.TokenEOF]), "Expected }")),
  )

  let res = {
    use tokens <- result.try(
      lexer.lex_program("if (!=fn *    })") |> result.map_error(LexerError),
    )

    parser.parse(parse_if(), tokens)
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
      lexer.lex_program("12321 + 43+ 324 +4+5345+1+++")
      |> result.map_error(LexerError),
    )

    parser.parse(parse_numbers(), tokens)
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
      lexer.lex_program("=={}+!=fn*+=={}+=={}+!=fn*")
      |> result.map_error(LexerError),
    )

    parser.parse(parse_equals(), tokens)
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
      lexer.lex_program("=={}+!=fn*+=={}+==}+!=fn*")
      |> result.map_error(LexerError),
    )

    parser.parse(parse_equals(), tokens)
    |> result.map_error(ParserError)
  }

  should.be_error(res)
  |> should.equal(
    ParserError(#(
      parser.Consumed([
        token.TokenRBrace,
        token.TokenPlus,
        token.TokenNotEquals,
        token.TokenFn,
        token.TokenStar,
        token.TokenEOF,
      ]),
      "Expected {",
    )),
  )
}
