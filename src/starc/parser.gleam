import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string

import starc/lexer/token.{Span}
import starc/parser/ast
import starc/parser/core.{
  type Message, type Parser, block_newline, die, eat, eat_exact, eat_newlines,
  expect, ignore_newline, many, map, maybe, oneof, parse, perform, pure, sep_by,
  try
}

pub type ParserError {
  Message(Message)
  NotParsed(String)
}

// ================= IDENTIFIER =================

fn parse_ident() -> Parser(ast.Identifier, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenIdent(_) -> True
        _ -> False
      }
    })
    |> expect("identifier")
  )

  let assert token.TokenIdent(name) = t.token_type
  pure(ast.Identifier(name:, span: t.span))
}

// ================= TYPE =================

fn parse_typeid() -> Parser(ast.TypeIdentifier, r) {
  oneof([parse_type_pointer(), parse_type_slice(), parse_type_name()])
}

fn parse_type_pointer() -> Parser(ast.TypeIdentifier, r) {
  use token <- perform(eat_exact(token.TokenStar))
  use typeid <- perform(parse_typeid())
  pure(ast.TypePointer(
    typeid:,
    span: Span(start: token.span.start, end: typeid.span.end)
  ))
}

fn parse_type_slice() -> Parser(ast.TypeIdentifier, r) {
  use token <- perform(eat_exact(token.TokenLSquare))
  use _ <- perform(eat_exact(token.TokenRSquare))
  use typeid <- perform(parse_typeid())
  pure(ast.TypeSlice(
    typeid:,
    span: Span(start: token.span.start, end: typeid.span.end)
  ))
}

fn parse_type_name() -> Parser(ast.TypeIdentifier, r) {
  parse_ident()
  |> expect("type name")
  |> map(fn(id) { ast.TypeName(name: id.name, span: id.span) })
}

// ================= EXPRESSION =================

fn parse_expression() -> Parser(ast.UntypedExpression, r) {
  parse_logical_or_expression()
}

fn parse_logical_or_expression() -> Parser(ast.UntypedExpression, r) {
  use expr <- perform(parse_logical_and_expression())

  use next <- perform(
    many({
      use t <- perform(
        eat(fn(t) {
          case t {
            token.TokenDoubleBar -> True
            _ -> False
          }
        })
      )
      use e <- perform(parse_logical_and_expression())
      pure(#(t, e))
    })
  )

  pure(
    list.fold(next, expr, fn(e1, x) {
      let #(token, e2) = x
      case token.token_type {
        token.TokenDoubleBar ->
          ast.UntypedOrExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        _ -> panic
      }
    })
  )
}

fn parse_logical_and_expression() -> Parser(ast.UntypedExpression, r) {
  use expr <- perform(parse_comparable_expression())

  use next <- perform(
    many({
      use t <- perform(
        eat(fn(t) {
          case t {
            token.TokenDoubleAmpersand -> True
            _ -> False
          }
        })
      )
      use e <- perform(parse_comparable_expression())
      pure(#(t, e))
    })
  )

  pure(
    list.fold(next, expr, fn(e1, x) {
      let #(token, e2) = x
      case token.token_type {
        token.TokenDoubleAmpersand ->
          ast.UntypedAndExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        _ -> panic
      }
    })
  )
}

fn parse_comparable_expression() -> Parser(ast.UntypedExpression, r) {
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
        })
      )
      use e <- perform(parse_additive_expression())
      pure(#(t, e))
    })
  )

  pure(
    list.fold(next, expr, fn(e1, x) {
      let #(token, e2) = x
      case token.token_type {
        token.TokenEquals ->
          ast.UntypedEQExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenNotEquals ->
          ast.UntypedNEQExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenLT ->
          ast.UntypedLTExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenLE ->
          ast.UntypedLEExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenGT ->
          ast.UntypedGTExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenGE ->
          ast.UntypedGEExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        _ -> panic
      }
    })
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
        })
      )
      use e <- perform(parse_multiplicative_expression())
      pure(#(t, e))
    })
  )

  pure(
    list.fold(next, expr, fn(e1, item) {
      let #(token, e2) = item
      case token.token_type {
        token.TokenPlus ->
          ast.UntypedAddExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenMinus ->
          ast.UntypedSubExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        _ -> panic
      }
    }),
  )
}

fn parse_multiplicative_expression() -> Parser(ast.UntypedExpression, r) {
  use expr <- perform(parse_postfix_expression())

  use next <- perform(
    many({
      use t <- perform(
        eat(fn(t) {
          case t {
            token.TokenStar -> True
            token.TokenSlash -> True
            _ -> False
          }
        })
      )
      use e <- perform(parse_postfix_expression())
      pure(#(t, e))
    })
  )

  pure(
    list.fold(next, expr, fn(e1, item) {
      let #(token, e2) = item
      case token.token_type {
        token.TokenStar ->
          ast.UntypedMulExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        token.TokenSlash ->
          ast.UntypedDivExpr(
            e1:,
            e2:,
            span: Span(start: ast.span_of(e1).start, end: ast.span_of(e2).end)
          )

        _ -> panic
      }
    })
  )
}

// TODO: multiple index
fn parse_postfix_expression() -> Parser(ast.UntypedExpression, r) {
  use left <- perform(parse_primary_expression())
  use index <- perform(
    maybe({
      use _ <- perform(eat_exact(token.TokenLSquare))
      use index <- perform(ignore_newline(parse_expression()))
      eat_newlines({
        use token <- perform(eat_exact(token.TokenRSquare))
        pure(#(index, token.span.end))
      })
    })
  )

  case index {
    None -> pure(left)

    Some(#(index, end)) ->
      pure(ast.UntypedIndexExpr(
        slice: left,
        index:,
        span: Span(start: ast.span_of(left).start, end:)
      ))
  }
}

fn parse_primary_expression() -> Parser(ast.UntypedExpression, r) {
  oneof([
    parse_int(),
    parse_bool(),
    parse_string(),
    parse_nested_expression(),
    parse_addrof_expression(),
    parse_deref_expression(),
    parse_neg_expression(),
    parse_not_expression(),
    parse_call_expression(),
    parse_var_expression(),
    parse_slice_expression()
  ])
}

fn parse_int() -> Parser(ast.UntypedExpression, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenInt(_) -> True
        _ -> False
      }
    })
    |> expect("int")
  )

  let assert token.TokenInt(n) = t.token_type
  pure(ast.UntypedIntExpr(value: n, span: t.span))
}

fn parse_bool() -> Parser(ast.UntypedExpression, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenBool(_) -> True
        _ -> False
      }
    })
    |> expect("bool")
  )

  let assert token.TokenBool(b) = t.token_type
  pure(ast.UntypedBoolExpr(value: b, span: t.span))
}

fn parse_string() -> Parser(ast.UntypedExpression, r) {
  use t <- perform(
    eat(fn(t) {
      case t {
        token.TokenString(_) -> True
        _ -> False
      }
    })
    |> expect("string")
  )

  let assert token.TokenString(s) = t.token_type
  pure(ast.UntypedStringExpr(value: s, span: t.span))
}

// FIXME: span
fn parse_nested_expression() -> Parser(ast.UntypedExpression, r) {
  use _ <- perform(eat_exact(token.TokenLParen))
  use expr <- perform(ignore_newline(parse_expression()))
  eat_newlines({
    use _ <- perform(eat_exact(token.TokenRParen))
    pure(expr)
  })
}

fn parse_addrof_expression() -> Parser(ast.UntypedExpression, r) {
  use token <- perform(eat_exact(token.TokenAmpersand))
  use expr <- perform(parse_postfix_expression())
  pure(ast.UntypedAddrOfExpr(
    e: expr,
    span: Span(start: token.span.start, end: ast.span_of(expr).end)
  ))
}

fn parse_deref_expression() -> Parser(ast.UntypedExpression, r) {
  use token <- perform(eat_exact(token.TokenStar))
  use expr <- perform(parse_primary_expression())
  pure(ast.UntypedDerefExpr(
    e: expr,
    span: Span(start: token.span.start, end: ast.span_of(expr).end)
  ))
}

fn parse_neg_expression() -> Parser(ast.UntypedExpression, r) {
  use token <- perform(eat_exact(token.TokenMinus))
  use expr <- perform(parse_primary_expression())
  pure(ast.UntypedNegExpr(
    e: expr,
    span: Span(start: token.span.start, end: ast.span_of(expr).end)
  ))
}

fn parse_not_expression() -> Parser(ast.UntypedExpression, r) {
  use token <- perform(eat_exact(token.TokenBang))
  use expr <- perform(parse_primary_expression())
  pure(ast.UntypedNotExpr(
    e: expr,
    span: Span(start: token.span.start, end: ast.span_of(expr).end)
  ))
}

// TODO: move this to postfix
fn parse_call_expression() -> Parser(ast.UntypedExpression, r) {
  use function <- perform(
    try({
      use function <- perform(parse_var_expression())
      use _ <- perform(eat_exact(token.TokenLParen))
      pure(function)
    })
  )

  use args <- perform(ignore_newline(sep_by(parse_expression(), eat_exact(token.TokenComma))))

  eat_newlines({
    use token <- perform(eat_exact(token.TokenRParen))

    pure(ast.UntypedCallExpression(
      f: function,
      args:,
      span: Span(start: ast.span_of(function).start, end: token.span.end)
    ))
  })
}

fn parse_var_expression() -> Parser(ast.UntypedExpression, r) {
  use id <- perform(parse_ident())
  pure(ast.UntypedVarExpr(id))
}

fn parse_slice_expression() -> Parser(ast.UntypedExpression, r) {
  use typeid <- perform(parse_type_slice())
  let assert ast.TypeSlice(typeid: elem_typeid, span: Span(start:, ..)) = typeid

  use _ <- perform(eat_exact(token.TokenLParen))
  use #(ptr, len) <- perform(
    ignore_newline({
      use ptr <- perform(parse_expression())
      use _ <- perform(eat_exact(token.TokenComma))
      use len <- perform(parse_expression())
      pure(#(ptr, len))
    })
  )
  eat_newlines({
    use token <- perform(eat_exact(token.TokenRParen))
    pure(ast.UntypedSliceExpr(
      ptr:,
      len:,
      elem_typeid:,
      span: Span(start:, end: token.span.end)
    ))
  })
}

// ================= STATEMENT =================

fn parse_statement() -> Parser(ast.UntypedStatement, r) {
  oneof([
    parse_return_statement() |> expect("return statement"),
    parse_assign_statement() |> expect("assign statement"),
    parse_define_statement() |> expect("define statement"),
    parse_call_statement() |> expect("call statement"),
    parse_if_statement() |> expect("if statement")
  ])
}

fn parse_return_statement() -> Parser(ast.UntypedStatement, r) {
  use _ <- perform(eat_exact(token.TokenReturn))
  block_newline(
    parse_expression()
    |> map(fn(e) { ast.UntypedReturnStatement(e) })
  )
}

fn parse_assign_statement() -> Parser(ast.UntypedStatement, r) {
  use cell <- perform(
    try({
      use cell <- perform(oneof([parse_var_expression(), parse_deref_expression()]))
      use _ <- perform(eat_exact(token.TokenAssign))
      pure(cell)
    })
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
    })
  )

  block_newline({
    use expr <- perform(parse_expression())
    pure(ast.UntypedDefineStatement(id: name, typeid:, expr:))
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
    })
  )

  use elseblock <- perform(
    maybe({
      use _ <- perform(eat_exact(token.TokenElse))
      parse_block()
    })
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

fn parse_parameter() -> Parser(List(#(ast.Identifier, ast.TypeIdentifier)), r) {
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
  use fn_token <- perform(eat_exact(token.TokenFn))

  use id <- perform(parse_ident())

  use _ <- perform(eat_exact(token.TokenLParen))
  use params <- perform(sep_by(parse_parameter(), eat_exact(token.TokenComma)))
  use rparen_token <- perform(eat_exact(token.TokenRParen))

  use ret <- perform(maybe(parse_typeid()))

  use body <- perform(parse_block())

  pure(ast.UntypedFunctionDeclaration(
    id:,
    parameters: list.flatten(params),
    result: ret,
    body:,
    span: Span(start: fn_token.span.start, end: rparen_token.span.end)
  ))
}

pub fn parse_program(tokens: List(token.Token)) -> Result(ast.UntypedProgram, ParserError) {
  let p = {
    use program <- perform(many(parse_declaration()))
    eat_newlines(pure(program))
  }

  case parse(p, tokens) {
    Ok(#(tree, [])) -> Ok(tree)

    Ok(#(_, tokens)) ->
      Error(NotParsed(
        "Not parsed: "
        <> list.map(tokens, fn(t) { t.token_type }) |> string.inspect()
      ))

    Error(msg) -> Error(Message(msg))
  }
}
