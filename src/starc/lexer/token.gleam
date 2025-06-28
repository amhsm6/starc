pub type Span {
  Span(start: Pos, end: Pos)
}

pub type Pos {
  Pos(line: Int, char: Int)
}

pub fn advance_chars(pos: Pos, n: Int) -> Pos {
  let Pos(line:, char:) = pos
  Pos(line:, char: char + n)
}

pub fn advance_lines(pos: Pos, n: Int) -> Pos {
  Pos(line: pos.line + n, char: 1)
}

pub type Token {
  Token(token_type: TokenType, span: Span)
}

// TODO: pretty print
pub type TokenType {
  TokenIdent(String)

  TokenInt(Int)
  TokenBool(Bool)
  TokenString(String)

  TokenLParen
  TokenRParen
  TokenLBrace
  TokenRBrace

  TokenPlus
  TokenMinus
  TokenStar
  TokenSlash
  TokenComma
  TokenBang
  TokenAmpersand

  TokenDoubleAmpersand
  TokenDoubleBar

  TokenEquals
  TokenNotEquals
  TokenLT
  TokenLE
  TokenGT
  TokenGE

  TokenDefine
  TokenAssign

  TokenFn
  TokenReturn

  TokenIf
  TokenElse
  TokenElseIf

  TokenNewline
}
