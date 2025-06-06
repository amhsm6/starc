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

pub fn some_test() {
  let assert Ok([
    ast.FunctionDeclaration(
      "foo",
      [],
      Some("int"),
      [
        ast.DefineStatement(ast.VarExpr("x"), Some("int"), ast.IntExpr(3)),
        ast.AssignStatement(ast.VarExpr("y"), ast.IntExpr(5)),
        ast.AssignStatement(
          ast.DerefExpr(ast.VarExpr("foo")),
          ast.AddExpr(
            ast.IntExpr(3),
            ast.MulExpr(ast.IntExpr(2), ast.IntExpr(5)),
          ),
        ),
        ast.EvalStatement(ast.CallExpression(
          ast.VarExpr("baz"),
          [
            ast.DerefExpr(ast.IntExpr(4)),
            ast.DerefExpr(ast.AddExpr(ast.IntExpr(4), ast.VarExpr("foo"))),
            ast.DerefExpr(ast.AddExpr(
              ast.IntExpr(4),
              ast.NotExpr(ast.BoolExpr(False)),
            )),
          ],
        )),
      ],
    ),
  ]) =
    lex_and_parse(
      "\nfn foo() int {\n x int := 3\n y = 5\n *foo = (3 + 2 * 5)\nbaz(*4,\n*(4 + foo),\n    *(4 + !false)) }\n\n",
    )

  let assert Ok([
    ast.FunctionDeclaration(
      "foo",
      [],
      Some("int"),
      [
        ast.DefineStatement(
          ast.VarExpr("foo"),
          None,
          ast.EQExpr(
            ast.AddExpr(
              ast.SubExpr(
                ast.AddExpr(
                  ast.IntExpr(3),
                  ast.MulExpr(ast.IntExpr(5), ast.IntExpr(6)),
                ),
                ast.DerefExpr(ast.BoolExpr(False)),
              ),
              ast.AddrOfExpr(ast.AddExpr(
                ast.SubExpr(ast.BoolExpr(False), ast.BoolExpr(True)),
                ast.VarExpr("bar"),
              )),
            ),
            ast.DerefExpr(ast.DerefExpr(ast.VarExpr("double_deref"))),
          ),
        ),
      ],
    ),
  ]) =
    lex_and_parse(
      "fn foo() int { foo := (\n3 + 5 * 6 - *false + &(false - true + bar)\n==\n **double_deref) }",
    )

  let assert Ok([
    ast.FunctionDeclaration(
      "foo",
      [],
      Some("int"),
      [
        ast.DefineStatement(
          ast.VarExpr("foo"),
          Some("float32"),
          ast.AddExpr(ast.IntExpr(3), ast.IntExpr(8)),
        ),
        ast.AssignStatement(ast.DerefExpr(ast.VarExpr("x")), ast.IntExpr(15)),
      ],
    ),
  ]) = lex_and_parse("fn foo() int { foo float32 := 3 + 8\n *x = 15 }")

  let assert Error(ParserError(
    "Error at 15: Expected TokenRBrace, but found TokenAssign",
  )) = lex_and_parse("fn foo() int { foo float32 := 3 + 8 *x = 15 }")

  Nil
}
