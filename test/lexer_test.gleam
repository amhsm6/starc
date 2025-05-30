import gleeunit/should
import gsparse/lexer

pub fn parse_int_test() {
  lexer.lex("123")
  |> should.be_ok()
  |> should.equal([lexer.TokenInt(123), lexer.TokenEOF])

  lexer.lex("   123    ")
  |> should.be_ok()
  |> should.equal([lexer.TokenInt(123), lexer.TokenEOF])

  lexer.lex(" \n\n\n\n 1\n23  \n\n\n  ")
  |> should.be_ok()
  |> should.equal([lexer.TokenInt(1), lexer.TokenInt(23), lexer.TokenEOF])

  lexer.lex(" \n\n\n\n 1;\n23  \n\n\n  ")
  |> should.be_error()
  |> should.equal(lexer.UnexpectedToken(lexer.Pos(5, 3), ";\n23 "))
}

pub fn strip_comment_test() {
  lexer.lex(
    "123 567 // asda adsasdh 1231hj3 ada \n 456 // adjsakdjsakd adsasd a\n// 123 123132 23123 // ///\n23",
  )
  |> should.be_ok()
  |> should.equal([
    lexer.TokenInt(123),
    lexer.TokenInt(567),
    lexer.TokenInt(456),
    lexer.TokenInt(23),
    lexer.TokenEOF,
  ])

  lexer.lex(
    "123 567 // asda adsasdh 1231hj3 ada \n 456 // adjsakdjsakd adsasd a\n// 123 123132 23123 // ///",
  )
  |> should.be_ok()
  |> should.equal([
    lexer.TokenInt(123),
    lexer.TokenInt(567),
    lexer.TokenInt(456),
    lexer.TokenEOF,
  ])
}

pub fn ident_test() {
  lexer.lex("foo")
  |> should.be_ok()
  |> should.equal([lexer.TokenIdent("foo"), lexer.TokenEOF])
}

pub fn arith_test() {
  lexer.lex("(2 + 4 / 613 - 102) + (423 * (232 + 4/1+2-3-5))")
  |> should.be_ok()
  |> should.equal([
    lexer.TokenLParen,
    lexer.TokenInt(2),
    lexer.TokenPlus,
    lexer.TokenInt(4),
    lexer.TokenSlash,
    lexer.TokenInt(613),
    lexer.TokenMinus,
    lexer.TokenInt(102),
    lexer.TokenRParen,
    lexer.TokenPlus,
    lexer.TokenLParen,
    lexer.TokenInt(423),
    lexer.TokenStar,
    lexer.TokenLParen,
    lexer.TokenInt(232),
    lexer.TokenPlus,
    lexer.TokenInt(4),
    lexer.TokenSlash,
    lexer.TokenInt(1),
    lexer.TokenPlus,
    lexer.TokenInt(2),
    lexer.TokenMinus,
    lexer.TokenInt(3),
    lexer.TokenMinus,
    lexer.TokenInt(5),
    lexer.TokenRParen,
    lexer.TokenRParen,
    lexer.TokenEOF,
  ])
}
