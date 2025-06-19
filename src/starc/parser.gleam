import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string

import starc/lexer/token
import starc/parser/ast
import starc/parser/core.{
  type Parser, Message, block_newline, die, eat, eat_exact, eat_newlines, expect,
  ignore_newline, many, map, maybe, oneof, parse, perform, pure, sep_by, try,
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

fn parse_typeid() -> Parser(ast.TypeId, r) {
  use ptr <- perform(maybe(eat_exact(token.TokenStar)))
  case ptr {
    None -> parse_ident() |> expect("type") |> map(ast.TypeName)
    Some(_) -> {
      parse_typeid()
      |> map(ast.TypePointer)
    }
  }
}

// ================= EXPRESSION =================

fn parse_expression() -> Parser(ast.UntypedExpression, r) {
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
        token.TokenEquals -> ast.UntypedEQExpr(e1, e2)
        token.TokenNotEquals -> ast.UntypedNEQExpr(e1, e2)
        token.TokenLT -> ast.UntypedLTExpr(e1, e2)
        token.TokenLE -> ast.UntypedLEExpr(e1, e2)
        token.TokenGT -> ast.UntypedGTExpr(e1, e2)
        token.TokenGE -> ast.UntypedGEExpr(e1, e2)
        _ -> panic
      }
    }),
  )
}

fn parse_additive_expression() -> Parser(ast.UntypedExpression, r) {
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
        token.TokenPlus -> ast.UntypedAddExpr(e1, e2)
        token.TokenMinus -> ast.UntypedSubExpr(e1, e2)
        _ -> panic
      }
    }),
  )
}

fn parse_multiplicative_expression() -> Parser(ast.UntypedExpression, r) {
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
        token.TokenStar -> ast.UntypedMulExpr(e1, e2)
        token.TokenSlash -> ast.UntypedDivExpr(e1, e2)
        _ -> panic
      }
    }),
  )
}

fn parse_primary_expression() -> Parser(ast.UntypedExpression, r) {
  oneof([
    parse_call_expression(),
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

fn parse_call_expression() -> Parser(ast.UntypedExpression, r) {
  use function <- perform(
    try({
      use function <- perform(parse_var_expression())
      use _ <- perform(eat_exact(token.TokenLParen))
      pure(function)
    }),
  )
  use args <- perform(
    ignore_newline(sep_by(parse_expression(), eat_exact(token.TokenComma))),
  )
  eat_newlines({
    use _ <- perform(eat_exact(token.TokenRParen))
    pure(ast.UntypedCallExpression(f: function, args:))
  })
}

fn parse_nested_expression() -> Parser(ast.UntypedExpression, r) {
  use _ <- perform(eat_exact(token.TokenLParen))
  use expr <- perform(ignore_newline(parse_expression()))
  eat_newlines({
    use _ <- perform(eat_exact(token.TokenRParen))
    pure(expr)
  })
}

fn parse_not_expression() -> Parser(ast.UntypedExpression, r) {
  use _ <- perform(eat_exact(token.TokenBang))
  use expr <- perform(parse_primary_expression())
  pure(ast.UntypedNotExpr(expr))
}

fn parse_addrof_expression() -> Parser(ast.UntypedExpression, r) {
  use _ <- perform(eat_exact(token.TokenAmpersand))
  use expr <- perform(parse_primary_expression())
  pure(ast.UntypedAddrOfExpr(expr))
}

fn parse_deref_expression() -> Parser(ast.UntypedExpression, r) {
  use _ <- perform(eat_exact(token.TokenStar))
  use expr <- perform(parse_primary_expression())
  pure(ast.UntypedDerefExpr(expr))
}

fn parse_var_expression() -> Parser(ast.UntypedExpression, r) {
  use id <- perform(parse_ident())
  pure(ast.UntypedVarExpr(id))
}

fn parse_int() -> Parser(ast.UntypedExpression, r) {
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
  pure(ast.UntypedIntExpr(n))
}

fn parse_bool() -> Parser(ast.UntypedExpression, r) {
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
  pure(ast.UntypedBoolExpr(b))
}

fn parse_string() -> Parser(ast.UntypedExpression, r) {
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
  pure(ast.UntypedStringExpr(s))
}

// ================= STATEMENT =================

fn parse_statement() -> Parser(ast.UntypedStatement, r) {
  oneof([
    parse_return_statement() |> expect("return statement"),
    parse_assign_statement() |> expect("assign statement"),
    parse_define_statement() |> expect("define statement"),
    parse_call_statement() |> expect("call statement"),
    parse_if_statement() |> expect("if statement"),
  ])
}

fn parse_return_statement() -> Parser(ast.UntypedStatement, r) {
  use _ <- perform(eat_exact(token.TokenReturn))
  block_newline(
    parse_expression()
    |> map(fn(e) { ast.UntypedReturnStatement(e) }),
  )
}

fn parse_assign_statement() -> Parser(ast.UntypedStatement, r) {
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
    pure(ast.UntypedAssignStatement(cell:, expr:))
  })
}

fn parse_define_statement() -> Parser(ast.UntypedStatement, r) {
  use #(name, typeid) <- perform(
    try({
      use name <- perform(parse_var_expression())
      let assert ast.UntypedVarExpr(name) = name

      use typeid <- perform(maybe(parse_typeid()))
      use _ <- perform(eat_exact(token.TokenDefine))
      pure(#(name, typeid))
    }),
  )

  block_newline({
    use expr <- perform(parse_expression())
    pure(ast.UntypedDefineStatement(name:, typeid:, expr:))
  })
}

fn parse_call_statement() -> Parser(ast.UntypedStatement, r) {
  eat_newlines({
    use expr <- perform(block_newline(parse_call_expression()))
    pure(ast.UntypedCallStatement(expr))
  })
}

fn parse_if_statement() -> Parser(ast.UntypedStatement, r) {
  use _ <- perform(eat_exact(token.TokenIf))
  use condition <- perform(block_newline(parse_expression()))
  use block <- perform(parse_block())

  use elseifs <- perform(
    many({
      use _ <- perform(eat_exact(token.TokenElseIf))
      use expr <- perform(block_newline(parse_expression()))
      use block <- perform(parse_block())
      pure(#(expr, block))
    }),
  )

  use elseblock <- perform(
    maybe({
      use _ <- perform(eat_exact(token.TokenElse))
      parse_block()
    }),
  )

  pure(ast.UntypedIfStatement(condition:, block:, elseifs:, elseblock:))
}

fn parse_block() -> Parser(ast.UntypedBlock, r) {
  use _ <- perform(eat_exact(token.TokenLBrace))
  use statements <- perform(many(parse_statement()))
  use _ <- perform(eat_exact(token.TokenRBrace))
  pure(statements)
}

// ================= DECLARATION =================

fn parse_declaration() -> Parser(ast.UntypedDeclaration, r) {
  oneof([parse_function_declaration()])
}

fn parse_parameter() -> Parser(List(#(ast.Identifier, ast.TypeId)), r) {
  use names <- perform(sep_by(parse_ident(), eat_exact(token.TokenComma)))

  case names {
    [] -> die()
    names -> {
      use typeid <- perform(parse_typeid())
      pure(list.map(names, pair.new(_, typeid)))
    }
  }
}

fn parse_function_declaration() -> Parser(ast.UntypedDeclaration, r) {
  use _ <- perform(eat_exact(token.TokenFn))

  use name <- perform(parse_ident())

  use _ <- perform(eat_exact(token.TokenLParen))
  use params <- perform(sep_by(parse_parameter(), eat_exact(token.TokenComma)))
  use _ <- perform(eat_exact(token.TokenRParen))

  use ret <- perform(maybe(parse_typeid()))

  use body <- perform(parse_block())

  pure(ast.UntypedFunctionDeclaration(
    name:,
    parameters: list.flatten(params),
    result: ret,
    body:,
  ))
}

pub fn parse_program(
  tokens: List(token.Token),
) -> Result(ast.UntypedProgram, String) {
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
