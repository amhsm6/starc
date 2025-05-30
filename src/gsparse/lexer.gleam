import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string

import gsparse/lexer/util

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

type Lexer(a, r) {
  Lexer(run: fn(LexerState, fn(LexerState, a) -> r, fn() -> r) -> r)
}

type LexerState {
  LexerState(input: String, pos: Pos)
}

pub type Pos {
  Pos(line: Int, char: Int)
}

pub type LexerError {
  UnexpectedToken(at: Pos, next: String)
}

fn advance_chars(pos: Pos, n: Int) -> Pos {
  let Pos(line, char) = pos
  Pos(line, char + n)
}

fn advance_char(pos: Pos) -> Pos {
  advance_chars(pos, 1)
}

fn advance_line(pos: Pos) -> Pos {
  Pos(pos.line + 1, 1)
}

fn after(l: Lexer(a, r), f: fn(a) -> Lexer(b, r)) -> Lexer(b, r) {
  use state, succ, fail <- Lexer
  l.run(state, fn(state, x) { f(x).run(state, succ, fail) }, fail)
}

fn pure(value: a) -> Lexer(a, r) {
  Lexer(fn(state, succ, _) { succ(state, value) })
}

fn die() -> Lexer(a, r) {
  Lexer(fn(_, _, fail) { fail() })
}

fn many(l: Lexer(a, r)) -> Lexer(List(a), r) {
  use state, succ, fail <- Lexer

  l.run(
    state,
    fn(state, element) {
      many(l).run(
        state,
        fn(state, rest) { succ(state, [element, ..rest]) },
        fail,
      )
    },
    fn() { succ(state, []) },
  )
}

fn some(l: Lexer(a, r)) -> Lexer(List(a), r) {
  use list <- after(many(l))
  case list {
    [] -> die()
    list -> pure(list)
  }
}

pub fn lex(input: String) -> Result(List(Token), LexerError) {
  let state = LexerState(input, Pos(1, 1))

  many(token()).run(
    state,
    fn(state, tokens) {
      let tokens =
        option.values(tokens)
        |> list.append([TokenEOF])

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

fn token() -> Lexer(Option(Token), r) {
  use state, succ, fail <- Lexer

  use <- symbol().run(state, succ)
  use <- int().run(state, succ)
  use <- ident().run(state, succ)
  fail()
}

fn symbol() -> Lexer(Option(Token), r) {
  use LexerState(input, pos), succ, fail <- Lexer

  case input {
    " " <> rest -> succ(LexerState(rest, advance_char(pos)), None)
    "\n" <> rest -> succ(LexerState(rest, advance_line(pos)), None)
    "//" <> rest -> {
      let rest =
        string.split_once(rest, "\n")
        |> result.map(pair.second)
        |> result.unwrap("")

      succ(LexerState(rest, advance_line(pos)), None)
    }

    "(" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenLParen))
    ")" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenRParen))

    "+" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenPlus))
    "-" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenMinus))
    "*" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenStar))
    "/" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenSlash))

    "==" <> rest -> {
      succ(LexerState(rest, advance_chars(pos, 2)), Some(TokenEquals))
    }
    "<=" <> rest -> succ(LexerState(rest, advance_chars(pos, 2)), Some(TokenLE))
    "<" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenLT))
    ">=" <> rest -> succ(LexerState(rest, advance_chars(pos, 2)), Some(TokenGE))
    ">" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenGT))
    "!=" <> rest -> {
      succ(LexerState(rest, advance_chars(pos, 2)), Some(TokenNotEquals))
    }

    "{" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenLBrace))
    "}" <> rest -> succ(LexerState(rest, advance_char(pos)), Some(TokenRBrace))

    "if" <> rest -> succ(LexerState(rest, advance_chars(pos, 2)), Some(TokenIf))
    "fn" <> rest -> succ(LexerState(rest, advance_chars(pos, 2)), Some(TokenFn))

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

  result.unwrap(res, fail())
}

fn ident() -> Lexer(Option(Token), r) {
  let alpha_underscore = set.union(util.alpha(), util.underscore())
  let alphanumeric_underscore = set.union(alpha_underscore, util.digits())

  use first <- after(char_in_set(alpha_underscore))
  use rest <- after(char_in_set(alphanumeric_underscore) |> many())
  pure(Some(TokenIdent(string.concat([first, ..rest]))))
}

fn digit() -> Lexer(Int, r) {
  use LexerState(input, pos), succ, fail <- Lexer

  let res = {
    use #(char, rest) <- result.try(string.pop_grapheme(input))
    use digit <- result.try(int.parse(char))
    Ok(succ(LexerState(rest, advance_char(pos)), digit))
  }

  result.unwrap(res, fail())
}

fn int() -> Lexer(Option(Token), r) {
  use digits <- after(digit() |> some())
  let assert Ok(num) = int.undigits(digits, 10)
  pure(Some(TokenInt(num)))
}
