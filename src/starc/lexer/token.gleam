pub type Token {
  TokenIdent(String)
  TokenInt(Int)

  TokenComma

  TokenLParen
  TokenRParen
  TokenPlus
  TokenMinus
  TokenStar
  TokenSlash

  TokenEquals
  TokenNotEquals
  TokenLT
  TokenLE
  TokenGT
  TokenGE

  TokenBang
  TokenAmpersand

  TokenLBrace
  TokenRBrace

  TokenIf
  TokenFn

  TokenEOF
}
