import gleeunit/should

import starc/lexer
import starc/lexer/core.{Pos}
import starc/lexer/token

pub fn parse_int_test() {
  lexer.lex("123")
  |> should.be_ok()
  |> should.equal([token.TokenInt(123), token.TokenEOF])

  lexer.lex("   123    ")
  |> should.be_ok()
  |> should.equal([token.TokenInt(123), token.TokenEOF])

  lexer.lex(" \n\n\n\n 1\n23  \n\n\n  ")
  |> should.be_ok()
  |> should.equal([token.TokenInt(1), token.TokenInt(23), token.TokenEOF])

  lexer.lex(" \n\n\n\n 1;\n23  \n\n\n  ")
  |> should.be_error()
  |> should.equal(lexer.UnexpectedToken(Pos(5, 3), ";\n23 "))
}

pub fn strip_comment_test() {
  lexer.lex(
    "123 567 // asda adsasdh 1231hj3 ada \n 456 // adjsakdjsakd adsasd a\n// 123 123132 23123 // ///\n23",
  )
  |> should.be_ok()
  |> should.equal([
    token.TokenInt(123),
    token.TokenInt(567),
    token.TokenInt(456),
    token.TokenInt(23),
    token.TokenEOF,
  ])

  lexer.lex(
    "123 567 // asda adsasdh 1231hj3 ada \n 456 // adjsakdjsakd adsasd a\n// 123 123132 23123 // ///",
  )
  |> should.be_ok()
  |> should.equal([
    token.TokenInt(123),
    token.TokenInt(567),
    token.TokenInt(456),
    token.TokenEOF,
  ])
}

pub fn ident_test() {
  lexer.lex("foo")
  |> should.be_ok()
  |> should.equal([token.TokenIdent("foo"), token.TokenEOF])
}

pub fn arith_test() {
  lexer.lex("(2 + 4 / 613 - 102) + (423 * (232 + 4/1+2-3-5))")
  |> should.be_ok()
  |> should.equal([
    token.TokenLParen,
    token.TokenInt(2),
    token.TokenPlus,
    token.TokenInt(4),
    token.TokenSlash,
    token.TokenInt(613),
    token.TokenMinus,
    token.TokenInt(102),
    token.TokenRParen,
    token.TokenPlus,
    token.TokenLParen,
    token.TokenInt(423),
    token.TokenStar,
    token.TokenLParen,
    token.TokenInt(232),
    token.TokenPlus,
    token.TokenInt(4),
    token.TokenSlash,
    token.TokenInt(1),
    token.TokenPlus,
    token.TokenInt(2),
    token.TokenMinus,
    token.TokenInt(3),
    token.TokenMinus,
    token.TokenInt(5),
    token.TokenRParen,
    token.TokenRParen,
    token.TokenEOF,
  ])
}
