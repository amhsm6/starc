import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string

import starc/lexer/core.{type Lexer, type Pos, Lexer, LexerState} as lexer
import starc/lexer/token.{type Token}
import starc/lexer/util

pub type LexerError {
  UnexpectedToken(at: Pos, next: String)
}

fn symbol() -> Lexer(Option(Token), r) {
  use LexerState(input, pos), succ, fail <- Lexer

  case input {
    " " <> rest -> succ(LexerState(rest, lexer.advance_char(pos)), None)
    "\n" <> rest -> succ(LexerState(rest, lexer.advance_line(pos)), None)
    "//" <> rest -> {
      let rest =
        string.split_once(rest, "\n")
        |> result.map(pair.second)
        |> result.unwrap("")

      succ(LexerState(rest, lexer.advance_line(pos)), None)
    }

    "(" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenLParen))
    ")" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenRParen))

    "+" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenPlus))
    "-" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenMinus))
    "*" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenStar))
    "/" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenSlash))

    "==" <> rest -> {
      succ(
        LexerState(rest, lexer.advance_chars(pos, 2)),
        Some(token.TokenEquals),
      )
    }
    "<=" <> rest ->
      succ(LexerState(rest, lexer.advance_chars(pos, 2)), Some(token.TokenLE))
    "<" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenLT))
    ">=" <> rest ->
      succ(LexerState(rest, lexer.advance_chars(pos, 2)), Some(token.TokenGE))
    ">" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenGT))
    "!=" <> rest -> {
      succ(
        LexerState(rest, lexer.advance_chars(pos, 2)),
        Some(token.TokenNotEquals),
      )
    }

    "{" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenLBrace))
    "}" <> rest ->
      succ(LexerState(rest, lexer.advance_char(pos)), Some(token.TokenRBrace))

    "if" <> rest ->
      succ(LexerState(rest, lexer.advance_chars(pos, 2)), Some(token.TokenIf))
    "fn" <> rest ->
      succ(LexerState(rest, lexer.advance_chars(pos, 2)), Some(token.TokenFn))

    _ -> fail()
  }
}

fn char_in_set(s: Set(String)) -> Lexer(String, r) {
  use LexerState(input, pos), succ, fail <- Lexer

  let res = {
    use #(char, rest) <- result.try(string.pop_grapheme(input))
    case set.contains(s, char) {
      True -> Ok(succ(LexerState(rest, lexer.advance_char(pos)), char))
      False -> Error(Nil)
    }
  }

  result.unwrap(res, fail())
}

fn ident() -> Lexer(Option(Token), r) {
  let alpha_underscore = set.union(util.alpha(), util.underscore())
  let alphanumeric_underscore = set.union(alpha_underscore, util.digits())

  use first <- lexer.perform(char_in_set(alpha_underscore))
  use rest <- lexer.perform(
    char_in_set(alphanumeric_underscore) |> lexer.many(),
  )
  lexer.pure(Some(token.TokenIdent(string.concat([first, ..rest]))))
}

fn digit() -> Lexer(Int, r) {
  use LexerState(input, pos), succ, fail <- Lexer

  let res = {
    use #(char, rest) <- result.try(string.pop_grapheme(input))
    use digit <- result.try(int.parse(char))
    Ok(succ(LexerState(rest, lexer.advance_char(pos)), digit))
  }

  result.unwrap(res, fail())
}

fn int() -> Lexer(Option(Token), r) {
  use digits <- lexer.perform(digit() |> lexer.some())
  let assert Ok(num) = int.undigits(digits, 10)
  lexer.pure(Some(token.TokenInt(num)))
}

fn token() -> Lexer(Option(Token), r) {
  use state, succ, fail <- Lexer

  use <- symbol().run(state, succ)
  use <- int().run(state, succ)
  use <- ident().run(state, succ)
  fail()
}

pub fn lex(input: String) -> Result(List(Token), LexerError) {
  let l = lexer.many(token())

  l.run(
    LexerState(input, lexer.begin()),
    fn(state, tokens) {
      let tokens =
        option.values(tokens)
        |> list.append([token.TokenEOF])

      case state {
        LexerState("", _) -> Ok(tokens)
        LexerState(input, pos) -> {
          Error(UnexpectedToken(pos, string.slice(input, 0, 5)))
        }
      }
    },
    fn() { panic },
  )
}
