pub type Token {
  TokenIdent(String)
  TokenInt(Int)

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

  TokenLBrace
  TokenRBrace

  TokenIf
  TokenFn

  TokenEOF
}
