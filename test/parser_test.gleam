import gleam/result
import gleeunit/should

import gsparse/lexer
import gsparse/parser

type GeneralError {
  LexerError(lexer.LexerError)
  ParserError(String)
}

pub fn parse_if_test() {
  let res = {
    use tokens <- result.try(
      lexer.lex("if (!=fn *    })") |> result.map_error(LexerError),
    )

    parser.parse_if().run(
      parser.Unconsumed(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(#(
    parser.Consumed([lexer.TokenRBrace, lexer.TokenRParen, lexer.TokenEOF]),
    Nil,
  ))

  let res = {
    use tokens <- result.try(
      lexer.lex("if   ( =={") |> result.map_error(LexerError),
    )

    parser.parse_if().run(
      parser.Unconsumed(tokens),
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

    parser.parse_if().run(
      parser.Unconsumed(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(#(
    parser.Consumed([lexer.TokenRBrace, lexer.TokenRParen, lexer.TokenEOF]),
    Nil,
  ))
}

pub fn many_test() {
  let res = {
    use tokens <- result.try(
      lexer.lex("12321 + 43+ 324 +4+5345+1+++") |> result.map_error(LexerError),
    )

    parser.parse_numbers().run(
      parser.Unconsumed(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(
    #(parser.Consumed([lexer.TokenPlus, lexer.TokenPlus, lexer.TokenEOF]), [
      12_321, 43, 324, 4, 5345, 1,
    ]),
  )

  let res = {
    use tokens <- result.try(
      lexer.lex("=={}+!=fn*+=={}+=={}+!=fn*") |> result.map_error(LexerError),
    )

    parser.parse_equals().run(
      parser.Unconsumed(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_ok(res)
  |> should.equal(
    #(parser.Consumed([lexer.TokenEOF]), [
      lexer.TokenEquals,
      lexer.TokenNotEquals,
      lexer.TokenEquals,
      lexer.TokenEquals,
      lexer.TokenNotEquals,
    ]),
  )
  let res = {
    use tokens <- result.try(
      lexer.lex("=={}+!=fn*+=={}+==}+!=fn*") |> result.map_error(LexerError),
    )

    parser.parse_equals().run(
      parser.Unconsumed(tokens),
      fn(state, x) { Ok(#(state, x)) },
      fn(_, x) { Error(x) },
    )
    |> result.map_error(ParserError)
  }

  should.be_error(res)
  |> should.equal(ParserError("Expected {"))
}
