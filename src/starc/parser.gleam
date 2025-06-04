import gleam/list
import gleam/pair
import gleam/string

import starc/lexer/token.{type Token}
import starc/parser/ast
import starc/parser/core.{type Parser} as parser

// ================= BASIC =================

fn parse_ident() -> Parser(ast.Identifier, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenIdent(_) -> True
        _ -> False
      }
    })
    |> parser.expect("identifier"),
  )

  let assert token.TokenIdent(id) = t
  parser.pure(id)
}

fn parse_type() -> Parser(ast.Type, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenIdent(_) -> True
        _ -> False
      }
    })
    |> parser.expect("type"),
  )

  let assert token.TokenIdent(id) = t
  parser.pure(id)
}

// ================= EXPRESSION =================

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
    list.fold(next, expr, fn(e1, x) {
      let #(token, e2) = x
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

fn parse_primary_expression() -> Parser(ast.Expression, r) {
  parser.oneof([
    parse_nested_expression(),
    parse_not_expression(),
    parse_addrof_expression(),
    parse_deref_expression(),
    parse_var_expression(),
    parse_int(),
    parse_bool(),
    parse_string(),
  ])
}

fn parse_nested_expression() -> Parser(ast.Expression, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenLParen))
  use expr <- parser.perform(parse_expression())
  use _ <- parser.perform(parser.eat_exact(token.TokenRParen))
  parser.pure(expr)
}

fn parse_not_expression() -> Parser(ast.Expression, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenBang))
  use expr <- parser.perform(parse_primary_expression())
  parser.pure(ast.NotExpr(expr))
}

fn parse_addrof_expression() -> Parser(ast.Expression, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenAmpersand))
  use expr <- parser.perform(parse_primary_expression())
  parser.pure(ast.AddrOfExpr(expr))
}

fn parse_deref_expression() -> Parser(ast.Expression, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenStar))
  use expr <- parser.perform(parse_primary_expression())
  parser.pure(ast.DerefExpr(expr))
}

fn parse_var_expression() -> Parser(ast.Expression, r) {
  use id <- parser.perform(parse_ident())
  parser.pure(ast.VarExpr(id))
}

fn parse_int() -> Parser(ast.Expression, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenInt(_) -> True
        _ -> False
      }
    }),
  )

  let assert token.TokenInt(n) = t
  parser.pure(ast.IntExpr(n))
}

fn parse_bool() -> Parser(ast.Expression, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenBool(_) -> True
        _ -> False
      }
    }),
  )

  let assert token.TokenBool(b) = t
  parser.pure(ast.BoolExpr(b))
}

fn parse_string() -> Parser(ast.Expression, r) {
  use t <- parser.perform(
    parser.eat(fn(t) {
      case t {
        token.TokenString(_) -> True
        _ -> False
      }
    }),
  )

  let assert token.TokenString(s) = t
  parser.pure(ast.StringExpr(s))
}

// ================= STATEMENT =================

fn parse_statement() -> Parser(ast.Statement, r) {
  parser.oneof([
    parse_assign_statement(),
    parse_define_statement(),
    parse_eval_statement(),
  ])
}

fn parse_assign_statement() -> Parser(ast.Statement, r) {
  use cell <- parser.perform(
    parser.try({
      use cell <- parser.perform(
        parser.oneof([parse_var_expression(), parse_deref_expression()]),
      )
      use _ <- parser.perform(parser.eat_exact(token.TokenAssign))
      parser.pure(cell)
    }),
  )
  use expr <- parser.perform(parse_expression())
  parser.pure(ast.AssignStatement(cell, expr))
}

fn parse_define_statement() -> Parser(ast.Statement, r) {
  use name <- parser.perform(parse_var_expression())
  use ty <- parser.perform(parser.maybe(parse_type()))
  use _ <- parser.perform(parser.eat_exact(token.TokenDefine))
  use expr <- parser.perform(parse_expression())
  parser.pure(ast.DefineStatement(name, ty, expr))
}

fn parse_eval_statement() -> Parser(ast.Statement, r) {
  use expr <- parser.perform(parse_expression())
  parser.pure(ast.EvalStatement(expr))
}

fn parse_block() -> Parser(ast.Block, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenLBrace))
  use statements <- parser.perform(parser.many(parse_statement()))
  use _ <- parser.perform(parser.eat_exact(token.TokenRBrace))
  parser.pure(statements)
}

// ================= DECLARATION =================

fn parse_declaration() -> Parser(ast.Declaration, r) {
  parser.oneof([parse_function_declaration()])
}

fn parse_parameter() -> Parser(List(#(ast.Identifier, ast.Type)), r) {
  use names <- parser.perform(parser.sep_by(
    parse_ident(),
    parser.eat_exact(token.TokenComma),
  ))
  use ty <- parser.perform(parse_type())
  parser.pure(list.map(names, pair.new(_, ty)))
}

fn parse_function_declaration() -> Parser(ast.Declaration, r) {
  use _ <- parser.perform(parser.eat_exact(token.TokenFn))

  use name <- parser.perform(parse_ident())

  use _ <- parser.perform(parser.eat_exact(token.TokenLParen))
  use params <- parser.perform(parser.sep_by(
    parse_parameter(),
    parser.eat_exact(token.TokenComma),
  ))
  use _ <- parser.perform(parser.eat_exact(token.TokenRParen))

  use ret <- parser.perform(parser.maybe(parse_type()))

  use body <- parser.perform(parse_block())

  parser.pure(ast.FunctionDeclaration(name, list.flatten(params), ret, body))
}

pub fn parse_program(tokens: List(Token)) -> Result(ast.Program, parser.Message) {
  let p = parser.many(parse_declaration())
  parser.parse(p, tokens)
}
