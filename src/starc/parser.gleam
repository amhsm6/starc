import gleam/int
import gleam/list
import gleam/pair
import gleam/string

import starc/lexer/token.{type Token}
import starc/parser/ast
import starc/parser/core.{
  type Parser, Message, block_newline, die, eat, eat_exact, eat_newlines, expect,
  ignore_newline, many, maybe, oneof, parse, perform, pure, sep_by, try,
}

// ================= BASIC =================

fn parse_ident() -> Parser(ast.Identifier, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenIdent(_) -> True
        _ -> False
      }
    })
    |> expect("identifier"),
  )

  let assert token.TokenIdent(id) = t
  pure(id)
}

fn parse_type() -> Parser(ast.Type, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenIdent(_) -> True
        _ -> False
      }
    })
    |> expect("type"),
  )

  let assert token.TokenIdent(id) = t
  pure(id)
}

// ================= EXPRESSION =================

fn parse_expression() -> Parser(ast.Expression, r) {
  use expr <- perform(parse_additive_expression())

  use next <- perform(
    many({
      use t <- perform(
        eat(fn(t) {
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
      use e <- perform(parse_additive_expression())
      pure(#(t, e))
    }),
  )

  pure(
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
  use expr <- perform(parse_multiplicative_expression())

  use next <- perform(
    many({
      use t <- perform(
        eat(fn(t) {
          case t {
            token.TokenPlus -> True
            token.TokenMinus -> True
            _ -> False
          }
        }),
      )
      use e <- perform(parse_multiplicative_expression())
      pure(#(t, e))
    }),
  )

  pure(
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
  use expr <- perform(parse_primary_expression())

  use next <- perform(
    many({
      use t <- perform(
        eat(fn(t) {
          case t {
            token.TokenStar -> True
            token.TokenSlash -> True
            _ -> False
          }
        }),
      )
      use e <- perform(parse_primary_expression())
      pure(#(t, e))
    }),
  )

  pure(
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
  oneof([
    parse_nested_expression(),
    parse_not_expression(),
    parse_addrof_expression(),
    parse_deref_expression(),
    parse_var_expression(),
    parse_int(),
    parse_bool(),
    parse_string(),
    parse_call_expression(),
  ])
}

fn parse_call_expression() -> Parser(ast.Expression, r) {
  use function <- perform(parse_var_expression())
  use _ <- perform(eat_exact(token.TokenLParen))
  use args <- perform(
    ignore_newline(sep_by(parse_expression(), eat_exact(token.TokenComma))),
  )
  eat_newlines({
    use _ <- perform(eat_exact(token.TokenRParen))
    pure(ast.CallExpression(f: function, args:))
  })
}

fn parse_nested_expression() -> Parser(ast.Expression, r) {
  use _ <- perform(eat_exact(token.TokenLParen))
  use expr <- perform(ignore_newline(parse_expression()))
  eat_newlines({
    use _ <- perform(eat_exact(token.TokenRParen))
    pure(expr)
  })
}

fn parse_not_expression() -> Parser(ast.Expression, r) {
  use _ <- perform(eat_exact(token.TokenBang))
  use expr <- perform(parse_primary_expression())
  pure(ast.NotExpr(expr))
}

fn parse_addrof_expression() -> Parser(ast.Expression, r) {
  use _ <- perform(eat_exact(token.TokenAmpersand))
  use expr <- perform(parse_primary_expression())
  pure(ast.AddrOfExpr(expr))
}

fn parse_deref_expression() -> Parser(ast.Expression, r) {
  use _ <- perform(eat_exact(token.TokenStar))
  use expr <- perform(parse_primary_expression())
  pure(ast.DerefExpr(expr))
}

fn parse_var_expression() -> Parser(ast.Expression, r) {
  use id <- perform(parse_ident())
  pure(ast.VarExpr(id))
}

fn parse_int() -> Parser(ast.Expression, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenInt(_) -> True
        _ -> False
      }
    })
    |> expect("int"),
  )

  let assert token.TokenInt(n) = t
  pure(ast.IntExpr(n))
}

fn parse_bool() -> Parser(ast.Expression, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenBool(_) -> True
        _ -> False
      }
    })
    |> expect("bool"),
  )

  let assert token.TokenBool(b) = t
  pure(ast.BoolExpr(b))
}

fn parse_string() -> Parser(ast.Expression, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenString(_) -> True
        _ -> False
      }
    })
    |> expect("string"),
  )

  let assert token.TokenString(s) = t
  pure(ast.StringExpr(s))
}

// ================= STATEMENT =================

fn parse_statement() -> Parser(ast.Statement, r) {
  oneof([
    parse_assign_statement() |> expect("assign statement"),
    parse_define_statement() |> expect("define statement"),
    parse_eval_statement() |> expect("eval statement"),
  ])
}

fn parse_assign_statement() -> Parser(ast.Statement, r) {
  use cell <- perform(
    try({
      use cell <- perform(
        oneof([parse_var_expression(), parse_deref_expression()]),
      )
      use _ <- perform(eat_exact(token.TokenAssign))
      pure(cell)
    }),
  )

  block_newline({
    use expr <- perform(parse_expression())
    pure(ast.AssignStatement(cell:, expr:))
  })
}

fn parse_define_statement() -> Parser(ast.Statement, r) {
  use #(name, ty) <- perform(
    try({
      use name <- perform(parse_var_expression())
      use ty <- perform(maybe(parse_type()))
      use _ <- perform(eat_exact(token.TokenDefine))
      pure(#(name, ty))
    }),
  )

  block_newline({
    use expr <- perform(parse_expression())
    pure(ast.DefineStatement(name:, ty:, expr:))
  })
}

fn parse_eval_statement() -> Parser(ast.Statement, r) {
  eat_newlines(
    block_newline({
      use expr <- perform(parse_call_expression())
      pure(ast.EvalStatement(expr))
    }),
  )
}

fn parse_block() -> Parser(ast.Block, r) {
  use _ <- perform(eat_exact(token.TokenLBrace))
  use statements <- perform(many(parse_statement()))
  use _ <- perform(eat_exact(token.TokenRBrace))
  pure(statements)
}

// ================= DECLARATION =================

fn parse_declaration() -> Parser(ast.Declaration, r) {
  oneof([parse_function_declaration()])
}

fn parse_parameter() -> Parser(List(#(ast.Identifier, ast.Type)), r) {
  use names <- perform(sep_by(parse_ident(), eat_exact(token.TokenComma)))

  case names {
    [] -> die()
    names -> {
      use ty <- perform(parse_type())
      pure(list.map(names, pair.new(_, ty)))
    }
  }
}

fn parse_function_declaration() -> Parser(ast.Declaration, r) {
  use _ <- perform(eat_exact(token.TokenFn))

  use name <- perform(parse_ident())

  use _ <- perform(eat_exact(token.TokenLParen))
  use params <- perform(sep_by(parse_parameter(), eat_exact(token.TokenComma)))
  use _ <- perform(eat_exact(token.TokenRParen))

  use ret <- perform(maybe(parse_type()))

  use body <- perform(parse_block())

  pure(ast.FunctionDeclaration(
    name:,
    parameters: list.flatten(params),
    result: ret,
    body:,
  ))
}

pub fn parse_program(tokens: List(Token)) -> Result(ast.Program, String) {
  let p = {
    use program <- perform(many(parse_declaration()))
    eat_newlines(pure(program))
  }

  case parse(p, tokens) {
    Ok(#(tree, [token.TokenEOF])) -> Ok(tree)

    Ok(#(_, tokens)) -> Error("Not parsed: " <> string.inspect(tokens))
    Error(Message(pos:, unexpected:, expected:)) -> {
      Error(
        "Error at "
        <> int.to_string(pos)
        <> ": Expected "
        <> string.join(expected, ", ")
        <> ", but found "
        <> unexpected,
      )
    }
  }
}
