import gleam/list
import gleam/pair
import gleam/string

import starc/lexer/token.{type Token}
import starc/parser/ast
import starc/parser/core.{type Parser} as parser

fn parse_ident() -> Parser(ast.Identifier, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenIdent(_) -> True
        _ -> False
      }
    })
    |> parser.with_message("Expected identifier"),
  )

  let assert token.TokenIdent(id) = t
  parser.pure(id)
}

fn parse_int() -> Parser(ast.Expression, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenInt(_) -> True
        _ -> False
      }
    })
    |> parser.with_message("Expected integer"),
  )

  let assert token.TokenInt(n) = t
  parser.pure(ast.IntExpr(n))
}

fn parse_expression() -> Parser(ast.Expression, r) {
  use expr <- parser.perform(parse_additive_expression())

  use next <- parser.perform(
    parser.many({
      use t <- parser.perform(
        parser.eat(fn(t) {
          case t {
            token.TokenEquals -> True
            token.TokenNotEquals -> True
            token.TokenLT -> True
            token.TokenLE -> True
            token.TokenGT -> True
            token.TokenGE -> True
            _ -> False
          }
        }),
      )
      use e <- parser.perform(parse_additive_expression())
      parser.pure(#(t, e))
    }),
  )

  parser.pure(
    list.fold(next, expr, fn(e1, item) {
      let #(token, e2) = item
      case token {
        token.TokenEquals -> ast.EQExpr(e1, e2)
        token.TokenNotEquals -> ast.NEQExpr(e1, e2)
        token.TokenLT -> ast.LTExpr(e1, e2)
        token.TokenLE -> ast.LEExpr(e1, e2)
        token.TokenGT -> ast.GTExpr(e1, e2)
        token.TokenGE -> ast.GEExpr(e1, e2)
        _ -> panic
      }
    }),
  )
}

fn parse_additive_expression() -> Parser(ast.Expression, r) {
  use expr <- parser.perform(parse_multiplicative_expression())

  use next <- parser.perform(
    parser.many({
      use t <- parser.perform(
        parser.eat(fn(t) {
          case t {
            token.TokenPlus -> True
            token.TokenMinus -> True
            _ -> False
          }
        }),
      )
      use e <- parser.perform(parse_multiplicative_expression())
      parser.pure(#(t, e))
    }),
  )

  parser.pure(
    list.fold(next, expr, fn(e1, item) {
      let #(token, e2) = item
      case token {
        token.TokenPlus -> ast.AddExpr(e1, e2)
        token.TokenMinus -> ast.SubExpr(e1, e2)
        _ -> panic
      }
    }),
  )
}

fn parse_multiplicative_expression() -> Parser(ast.Expression, r) {
  use expr <- parser.perform(parse_primary_expression())

  use next <- parser.perform(
    parser.many({
      use t <- parser.perform(
        parser.eat(fn(t) {
          case t {
            token.TokenStar -> True
            token.TokenSlash -> True
            _ -> False
          }
        }),
      )
      use e <- parser.perform(parse_primary_expression())
      parser.pure(#(t, e))
    }),
  )

  parser.pure(
    list.fold(next, expr, fn(e1, item) {
      let #(token, e2) = item
      case token {
        token.TokenStar -> ast.MulExpr(e1, e2)
        token.TokenSlash -> ast.DivExpr(e1, e2)
        _ -> panic
      }
    }),
  )
}

fn parse_nested_expression() -> Parser(ast.Expression, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenLParen))
  use expr <- parser.perform(parse_expression())
  use _ <- parser.perform(parser.eat_exact(token.TokenRParen))
  parser.pure(expr)
}

fn parse_primary_expression() -> Parser(ast.Expression, r) {
  parser.oneof([parse_nested_expression(), parse_int()], "expected expression")
}

fn parse_statement() -> Parser(ast.Statement, r) {
  parse_expression()
  |> parser.perform(fn(x) { parser.pure(ast.EvalStatement(x)) })
}

fn parse_block() -> Parser(ast.Block, r) {
  use _ <- parser.perform(
    parser.eat_exact(token.TokenLBrace) |> parser.with_message("Expected {"),
  )
  use statements <- parser.perform(parser.many(parse_statement()))
  use _ <- parser.perform(
    parser.eat_exact(token.TokenRBrace) |> parser.with_message("Expected }"),
  )
  parser.pure(statements)
}

fn parse_parameter() -> Parser(List(#(ast.Identifier, ast.Type)), r) {
  use names <- parser.perform(parser.sep_by(
    parse_ident(),
    parser.eat_exact(token.TokenComma),
  ))
  use ty <- parser.perform(
    parse_ident() |> parser.with_message("Expected type"),
  )
  parser.pure(list.map(names, pair.new(_, ty)))
}

fn parse_function_declaration() -> Parser(ast.Declaration, r) {
  use _ <- parser.perform(
    parser.eat_exact(token.TokenFn) |> parser.with_message("Expected fn"),
  )

  use name <- parser.perform(
    parse_ident() |> parser.with_message("Expected identifier"),
  )

  use _ <- parser.perform(
    parser.eat_exact(token.TokenLParen) |> parser.with_message("Expected ("),
  )
  use params <- parser.perform(parser.sep_by(
    parse_parameter(),
    parser.eat_exact(token.TokenComma),
  ))
  use _ <- parser.perform(
    parser.eat_exact(token.TokenRParen) |> parser.with_message("Expected )"),
  )

  use ret <- parser.perform(
    parse_ident() |> parser.with_message("Expected type") |> parser.maybe(),
  )

  use body <- parser.perform(parse_block())

  parser.pure(ast.FunctionDeclaration(name, list.flatten(params), ret, body))
}

fn parse_declaration() -> Parser(ast.Declaration, r) {
  parser.oneof([parse_function_declaration()], "Expected declaration")
}

pub fn parse_program(tokens: List(Token)) -> Result(ast.Program, String) {
  let p = parser.many(parse_declaration())
  case parser.parse(p, tokens) {
    Ok(#(state, tree)) if state.input == [token.TokenEOF] -> Ok(tree)
    Ok(#(state, _)) -> Error("Not parsed: " <> string.inspect(state.input))
    Error(#(state, msg)) -> Error(msg <> ": " <> string.inspect(state.input))
  }
}
