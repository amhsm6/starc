import gleam/option.{None, Some}
import gleam/result

import starc/lexer
import starc/parser
import starc/parser/ast

type Error {
  LexerError(lexer.LexerError)
  ParserError(String)
}

fn lex_and_parse(input: String) -> Result(ast.Program, Error) {
  use tokens <- result.try(
    lexer.lex_program(input)
    |> result.map_error(LexerError),
  )

  use tree <- result.try(
    parser.parse_program(tokens)
    |> result.map_error(ParserError),
  )

  Ok(tree)
}

pub fn parse_fn_test() {
  let assert Ok([ast.FunctionDeclaration("foo", [], Some("float32"), [])]) =
    lex_and_parse("fn foo() float32 {}")

  let assert Ok([ast.FunctionDeclaration("foo", [#("x", "int")], None, [])]) =
    lex_and_parse("fn foo(x int) {}")

  let assert Ok([
    ast.FunctionDeclaration("foo", [#("x", "int"), #("y", "int")], None, []),
  ]) = lex_and_parse("fn foo(x, y int) {}")

  let assert Ok([
    ast.FunctionDeclaration(
      "foo",
      [
        #("a", "int"),
        #("b", "float"),
        #("c", "float"),
        #("d", "float"),
        #("f", "int32"),
      ],
      None,
      [],
    ),
  ]) = lex_and_parse("fn foo(a int, b, c, d float, f int32) {}")

  let assert Error(ParserError(
    "Error at 3: Expected TokenLParen, but found TokenRParen",
  )) = lex_and_parse("fn foo) {}")

  let assert Error(ParserError(
    "Error at 4: Expected identifier, TokenRParen, but found TokenComma",
  )) = lex_and_parse("fn foo(,) {}")

  let assert Error(ParserError(
    "Error at 7: Expected type, but found TokenRParen",
  )) = lex_and_parse("fn foo(x,y) {}")

  let assert Error(ParserError(
    "Error at 5: Expected type, TokenLBrace, but found TokenRBrace",
  )) = lex_and_parse("fn foo() }")

  Nil
}
