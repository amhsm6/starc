pub type Token {
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

  TokenEquals
  TokenNotEquals
  TokenLT
  TokenLE
  TokenGT
  TokenGE

  TokenDefine
  TokenAssign

  TokenIf
  TokenFn

  TokenEOF
}
