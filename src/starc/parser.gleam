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

fn parse_statement() -> Parser(ast.Statement, r) {
  parser.die("todo")
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

  use ret <- parser.perform(parser.maybe(
    parse_ident() |> parser.with_message("Expected type"),
  ))

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
