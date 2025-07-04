import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string

import starc/lexer/core.{
  type Lexer, LexerState, eat, eat_exact, extract_span, lex, many, map, oneof,
  perform, pure, replace, some,
}
import starc/lexer/token.{type Pos, type Token, type TokenType, Token}
import starc/lexer/util

pub type LexerError {
  UnexpectedToken(at: Pos, next: String)
}

fn to_token(l: Lexer(a, r), token_type: TokenType) -> Lexer(Option(Token), r) {
  l
  |> extract_span()
  |> map(fn(x) { Some(Token(token_type:, span: pair.second(x))) })
}

fn comment() -> Lexer(Option(Token), r) {
  use _ <- perform(eat_exact("//"))
  use _ <- perform(many(eat(fn(x) { x != "\n" })))
  eat_exact("\n") |> replace(None)
}

fn whitespace() -> Lexer(Option(Token), r) {
  oneof([
    eat_exact(" ") |> replace(None),
    eat_exact("\n") |> to_token(token.TokenNewline),
  ])
}

fn symbol() -> Lexer(Option(Token), r) {
  oneof([
    eat_exact("(") |> to_token(token.TokenLParen),
    eat_exact(")") |> to_token(token.TokenRParen),
    eat_exact("{") |> to_token(token.TokenLBrace),
    eat_exact("}") |> to_token(token.TokenRBrace),
    eat_exact("[") |> to_token(token.TokenLSquare),
    eat_exact("]") |> to_token(token.TokenRSquare),
    eat_exact("!=") |> to_token(token.TokenNotEquals),
    eat_exact("&&") |> to_token(token.TokenDoubleAmpersand),
    eat_exact("||") |> to_token(token.TokenDoubleBar),
    eat_exact("+") |> to_token(token.TokenPlus),
    eat_exact("-") |> to_token(token.TokenMinus),
    eat_exact("*") |> to_token(token.TokenStar),
    eat_exact("/") |> to_token(token.TokenSlash),
    eat_exact(",") |> to_token(token.TokenComma),
    eat_exact("!") |> to_token(token.TokenBang),
    eat_exact("&") |> to_token(token.TokenAmpersand),
    eat_exact("==") |> to_token(token.TokenEquals),
    eat_exact("<=") |> to_token(token.TokenLE),
    eat_exact("<") |> to_token(token.TokenLT),
    eat_exact(">=") |> to_token(token.TokenGE),
    eat_exact(">") |> to_token(token.TokenGT),
    eat_exact(":=") |> to_token(token.TokenDefine),
    eat_exact("=") |> to_token(token.TokenAssign),
    eat_exact("fn") |> to_token(token.TokenFn),
    eat_exact("return") |> to_token(token.TokenReturn),
    eat_exact("if") |> to_token(token.TokenIf),
    eat_exact("else if") |> to_token(token.TokenElseIf),
    eat_exact("else") |> to_token(token.TokenElse),
  ])
}

fn ident() -> Lexer(Option(Token), r) {
  let alpha_underscore = set.union(util.alpha(), util.underscore())
  let alphanumeric_underscore = set.union(alpha_underscore, util.digits())

  {
    use first <- perform(eat(set.contains(alpha_underscore, _)))
    use rest <- perform(many(eat(set.contains(alphanumeric_underscore, _))))
    pure(string.concat([first, ..rest]))
  }
  |> extract_span()
  |> map(fn(x) {
    let #(id, span) = x
    Some(Token(token_type: token.TokenIdent(id), span:))
  })
}

fn digit() -> Lexer(Int, r) {
  use c <- perform(eat(fn(c) { int.parse(c) |> result.is_ok() }))

  let assert Ok(n) = int.parse(c)
  pure(n)
}

fn int() -> Lexer(Option(Token), r) {
  {
    use digits <- perform(some(digit()))
    let assert Ok(num) = int.undigits(digits, 10)
    pure(num)
  }
  |> extract_span()
  |> map(fn(x) {
    let #(num, span) = x
    Some(Token(token_type: token.TokenInt(num), span:))
  })
}

fn bool() -> Lexer(Option(Token), r) {
  oneof([
    eat_exact("true") |> to_token(token.TokenBool(True)),
    eat_exact("false") |> to_token(token.TokenBool(False)),
  ])
}

fn string() -> Lexer(Option(Token), r) {
  {
    use _ <- perform(eat_exact("\""))
    use str <- perform(eat(fn(c) { c != "\"" }))
    use _ <- perform(eat_exact("\""))
    pure(str)
  }
  |> extract_span()
  |> map(fn(x) {
    let #(str, span) = x
    Some(Token(token_type: token.TokenString(str), span:))
  })
}

fn token() -> Lexer(Option(Token), r) {
  oneof([comment(), whitespace(), symbol(), bool(), ident(), int(), string()])
}

pub fn lex_program(input: String) -> Result(List(Token), LexerError) {
  let l = many(token())
  let assert Some(#(state, tokens)) = lex(l, input)

  case state {
    LexerState(input: "", ..) -> Ok(option.values(tokens))

    LexerState(input:, pos:) ->
      Error(UnexpectedToken(at: pos, next: string.slice(input, 0, 5)))
  }
}
