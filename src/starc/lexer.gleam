import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string

import starc/lexer/core.{
  type Lexer, type Pos, Lexer, LexerState, advance_char, advance_chars,
  advance_line, lex, many, perform, pure, some,
}
import starc/lexer/token.{type Token}
import starc/lexer/util

pub type LexerError {
  UnexpectedToken(at: Pos, next: String)
}

fn symbol() -> Lexer(Option(Token), r) {
  use LexerState(input, pos), succ, fail <- Lexer

  case input {
    " " <> rest -> succ(LexerState(rest, advance_char(pos)), None)
    "\n" <> rest ->
      succ(LexerState(rest, advance_line(pos)), Some(token.TokenNewline))
    "//" <> rest -> {
      let rest =
        string.split_once(rest, "\n")
        |> result.map(pair.second)
        |> result.unwrap("")

      succ(LexerState(rest, advance_line(pos)), None)
    }

    "(" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenLParen))
    ")" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenRParen))
    "{" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenLBrace))
    "}" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenRBrace))

    "!=" <> rest -> {
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenNotEquals))
    }

    "&&" <> rest ->
      succ(
        LexerState(rest, advance_chars(pos, 2)),
        Some(token.TokenDoubleAmpersand),
      )

    "||" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenDoubleBar))

    "+" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenPlus))
    "-" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenMinus))
    "*" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenStar))
    "/" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenSlash))
    "," <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenComma))
    "!" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenBang))
    "&" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenAmpersand))

    "==" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenEquals))
    "<=" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenLE))
    "<" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenLT))
    ">=" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenGE))
    ">" <> rest ->
      succ(LexerState(rest, advance_char(pos)), Some(token.TokenGT))

    ":=" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenDefine))
    "=" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenAssign))

    "fn" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenFn))
    "return" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 6)), Some(token.TokenReturn))

    "if" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 2)), Some(token.TokenIf))
    "else if" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 7)), Some(token.TokenElseIf))
    "else" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 4)), Some(token.TokenElse))

    _ -> fail()
  }
}

fn char_in_set(s: Set(String)) -> Lexer(String, r) {
  use LexerState(input, pos), succ, fail <- Lexer

  let res = {
    use #(char, rest) <- result.try(string.pop_grapheme(input))
    case set.contains(s, char) {
      True -> Ok(succ(LexerState(rest, advance_char(pos)), char))
      False -> Error(Nil)
    }
  }

  result.lazy_unwrap(res, fail)
}

fn ident() -> Lexer(Option(Token), r) {
  let alpha_underscore = set.union(util.alpha(), util.underscore())
  let alphanumeric_underscore = set.union(alpha_underscore, util.digits())

  use first <- perform(char_in_set(alpha_underscore))
  use rest <- perform(char_in_set(alphanumeric_underscore) |> many())
  pure(Some(token.TokenIdent(string.concat([first, ..rest]))))
}

fn digit() -> Lexer(Int, r) {
  use LexerState(input, pos), succ, fail <- Lexer

  let res = {
    use #(char, rest) <- result.try(string.pop_grapheme(input))
    use digit <- result.try(int.parse(char))
    Ok(succ(LexerState(rest, advance_char(pos)), digit))
  }

  result.lazy_unwrap(res, fail)
}

fn int() -> Lexer(Option(Token), r) {
  use digits <- perform(some(digit()))
  let assert Ok(num) = int.undigits(digits, 10)
  pure(Some(token.TokenInt(num)))
}

fn bool() -> Lexer(Option(Token), r) {
  use LexerState(input, pos), succ, fail <- Lexer

  case input {
    "true" <> rest ->
      succ(LexerState(rest, advance_chars(pos, 4)), Some(token.TokenBool(True)))

    "false" <> rest ->
      succ(
        LexerState(rest, advance_chars(pos, 5)),
        Some(token.TokenBool(False)),
      )

    _ -> fail()
  }
}

fn string() -> Lexer(Option(Token), r) {
  use LexerState(input, pos), succ, fail <- Lexer

  let res = {
    use #(str, rest) <- result.try(case input {
      "\"" <> rest -> {
        string.split_once(rest, "\"")
      }
      _ -> Error(Nil)
    })

    Ok(succ(
      LexerState(rest, advance_chars(pos, string.length(str) + 2)),
      Some(token.TokenString(str)),
    ))
  }

  result.lazy_unwrap(res, fail)
}

fn token() -> Lexer(Option(Token), r) {
  use state, succ, fail <- Lexer

  use <- symbol().run(state, succ)
  use <- int().run(state, succ)
  use <- bool().run(state, succ)
  use <- string().run(state, succ)
  use <- ident().run(state, succ)
  fail()
}

pub fn lex_program(input: String) -> Result(List(Token), LexerError) {
  let l = many(token())
  let assert Some(#(state, tokens)) = lex(l, input)

  let tokens =
    option.values(tokens)
    |> list.append([token.TokenEOF])

  case state {
    LexerState("", _) -> Ok(tokens)
    LexerState(input, pos) -> {
      Error(UnexpectedToken(pos, string.slice(input, 0, 5)))
    }
  }
}
